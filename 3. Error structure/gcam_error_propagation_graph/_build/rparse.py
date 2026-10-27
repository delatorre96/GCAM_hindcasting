"""Minimal reader for R XDR serialization (save()/saveRDS v2/v3), enough for rgcam .dat projects."""
import lzma, struct, sys
import numpy as np


class Sym(str):
    pass


class RObj:
    def __init__(self, typ, value, attrs=None):
        self.typ, self.value, self.attrs = typ, value, attrs or {}

    def __repr__(self):
        return f"RObj({self.typ}, attrs={list(self.attrs)})"


class Reader:
    def __init__(self, buf):
        self.b = memoryview(buf)
        self.p = 0
        self.refs = []

    def i32(self):
        v = struct.unpack_from(">i", self.b, self.p)[0]
        self.p += 4
        return v

    def length(self):
        n = self.i32()
        if n == -1:
            hi, lo = self.i32(), self.i32()
            n = (hi << 32) + lo
        return n

    def pairlist_to_dict(self, pl):
        out = {}
        while isinstance(pl, RObj) and pl.typ == "pairlist":
            for tag, val in pl.value:
                out[tag] = val
            break
        return out

    def item(self):
        flags = self.i32()
        t = flags & 0xFF
        has_attr = flags & (1 << 9)
        has_tag = flags & (1 << 10)
        if t == 254:  # NILVALUE
            return None
        if t in (253, 252, 251, 250, 242, 241):
            return RObj("special", t)
        if t == 255:  # REF
            idx = flags >> 8
            if idx == 0:
                idx = self.i32()
            return self.refs[idx - 1]
        if t in (247, 249, 248):  # persist / namespace / package
            self.i32()  # 0
            n = self.i32()
            vals = [self.item() for _ in range(n)]
            o = RObj("ns", vals)
            self.refs.append(o)
            return o
        if t == 1:  # SYMSXP
            s = Sym(self.item())
            self.refs.append(s)
            return s
        if t in (2, 6, 239, 240, 3, 5, 17):  # pairlist-like
            items = []
            attrs = None
            while True:
                if has_attr:
                    attrs = self.item()
                tag = self.item() if has_tag else None
                car = self.item()
                items.append((tag, car))
                # cdr: peek flags for continuing pairlist
                save = self.p
                f2 = self.i32()
                t2 = f2 & 0xFF
                if t2 in (2, 239) and t in (2, 239):
                    flags, has_attr, has_tag = f2, f2 & (1 << 9), f2 & (1 << 10)
                    continue
                self.p = save
                cdr = self.item()
                if cdr is not None:
                    items.append(("__cdr__", cdr))
                break
            return RObj("pairlist", items)
        if t == 4:  # ENVSXP
            o = RObj("env", None)
            self.refs.append(o)
            self.i32()
            enclos = self.item(); frame = self.item(); hashtab = self.item(); attr = self.item()
            o.value = (frame, hashtab)
            return o
        if t == 9:  # CHARSXP
            n = self.i32()
            if n == -1:
                return None
            s = bytes(self.b[self.p:self.p + n]).decode("utf-8", "replace")
            self.p += n
            return s
        if t in (10, 13):
            n = self.length()
            arr = np.frombuffer(self.b, dtype=">i4", count=n, offset=self.p).astype(np.int64)
            self.p += 4 * n
            val = arr
            typ = "lgl" if t == 10 else "int"
        elif t == 14:
            n = self.length()
            val = np.frombuffer(self.b, dtype=">f8", count=n, offset=self.p).astype(np.float64)
            self.p += 8 * n
            typ = "real"
        elif t == 15:
            n = self.length()
            self.p += 16 * n
            val, typ = None, "cplx"
        elif t == 16:
            n = self.length()
            val = [self.item() for _ in range(n)]
            typ = "str"
        elif t in (19, 20):
            n = self.length()
            val = [self.item() for _ in range(n)]
            typ = "list"
        elif t == 24:
            n = self.length()
            val = bytes(self.b[self.p:self.p + n]); self.p += n
            typ = "raw"
        elif t == 25:
            val, typ = None, "s4"
        elif t == 238:  # ALTREP
            info = self.item()
            state = self.item()
            attr = self.item()
            cls = info.value[0][1] if isinstance(info, RObj) else None
            cls = str(cls)
            if cls == "compact_intseq":
                n, start, step = state.value
                val = np.arange(int(start), int(start) + int(n) * int(step), int(step))
                o = RObj("int", val)
            elif cls == "compact_realseq":
                n, start, step = state.value
                o = RObj("real", start + step * np.arange(int(n)))
            elif cls.startswith("wrap_"):
                o = state.value[0] if isinstance(state, RObj) else state
            elif cls == "deferred_string":
                arg = state.value[0][1]
                o = RObj("str", [str(x) for x in arg.value])
            else:
                raise ValueError(f"altrep {cls}")
            if attr is not None and isinstance(o, RObj):
                o.attrs = self.attrs_from(attr)
            return o
        else:
            raise ValueError(f"type {t} at {self.p}")
        attrs = self.attrs_from(self.item()) if has_attr else {}
        return RObj(typ, val, attrs)

    def attrs_from(self, pl):
        out = {}
        if isinstance(pl, RObj) and pl.typ == "pairlist":
            for tag, v in pl.value:
                out[str(tag)] = v
        return out


def load(path):
    raw = lzma.open(path).read()
    assert raw[:5] == b"RDX3\n"
    r = Reader(raw)
    r.p = 5
    assert bytes(r.b[r.p:r.p + 2]) == b"X\n"; r.p += 2
    ver = r.i32(); r.i32(); r.i32()
    if ver == 3:
        n = r.i32(); r.p += n
    return r.item()


def to_df(obj):
    import pandas as pd
    names = obj.attrs["names"].value
    cols = {}
    for nm, col in zip(names, obj.value):
        if col.typ == "int" and "levels" in col.attrs:
            lev = np.array(col.attrs["levels"].value, dtype=object)
            cols[nm] = lev[col.value - 1]
        elif col.typ == "str":
            cols[nm] = col.value
        else:
            cols[nm] = col.value
    return pd.DataFrame(cols)
