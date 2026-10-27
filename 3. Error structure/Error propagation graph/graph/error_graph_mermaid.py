"""
Grafo sectorial de GCAM-Europe -> Mermaid, con módulos y traza del error
(versión Python de error_graph_mermaid.R; misma lógica y mismos parámetros)

  1. Vecindario de orden k alrededor de un nodo semilla.
  2. Módulos (Louvain): grupos de sectores muy conectados entre sí y con pocas
     aristas hacia fuera. Cada módulo es un `subgraph` de Mermaid.
  3. Se DESPLIEGAN los módulos que contienen la semilla o la traza del error
     (o los que pidas con --expand); el resto se PLIEGA en una caja.
  4. Calor: color del nodo según |error| (inferno); aristas rojas (sobreestima)
     o azules (subestima), con grosor según |error| del flujo.
  5. Traza (naranja): desde la semilla se sigue al consumidor que más aporta al
     error de demanda de cada nodo (LMDI exacto): camino del error heredado.

Error: e = ln(value_chY / value_ref), mostrado como 100*(exp(e)-1) %.

Uso:
  python error_graph_mermaid.py "Iberian_Peninsula|electricity" --order 4
  python error_graph_mermaid.py "Spain|delivered gas" --order 3 --resolution 1.5
  python error_graph_mermaid.py "Iberian_Peninsula|electricity" --order 4 \
         --expand "Portugal|iron and steel" --max-members 14

Requiere: pandas, numpy, networkx (>= 2.8 para louvain_communities).
Escribe <out>.mmd y <out>.html (el HTML carga Mermaid desde un CDN).
"""
import argparse
import json
import os
import re

import networkx as nx
import numpy as np
import pandas as pd

BASE_DIR = os.path.dirname(os.path.abspath(__file__))


# ---------------------------------------------------------------- helpers
def lnr(a, b):
    a = np.asarray(a, float); b = np.asarray(b, float)
    ok = np.isfinite(a) & np.isfinite(b) & (a > 0) & (b > 0)
    out = np.full(a.shape, np.nan)
    out[ok] = np.log(a[ok] / b[ok])
    return out


def lmean(a, b):
    if not (np.isfinite(a) and np.isfinite(b)) or a <= 0 or b <= 0:
        return np.nan
    return a if abs(a - b) <= 1e-12 * max(a, b) else (a - b) / np.log(a / b)


def pct(e):
    return 100 * (np.exp(e) - 1)


def esc(s):
    return str(s).replace('"', "'")


def heat_class(p, bins=(2, 5, 10, 20, 40)):
    if p is None or not np.isfinite(p):
        return "hna"
    return "h" + str(int(np.searchsorted(bins, abs(p), side="right")))


FAMILIES = [("resid", "residencial"), ("comm", "terciario"), ("elect_td", "distribución eléctrica"),
            ("trn_", "transporte"), ("regional electricity", "red eléctrica"), ("electricity", "electricidad"),
            ("generation", "generación"), ("gas", "gas"), ("refin", "refino/líquidos"),
            ("liquids", "refino/líquidos"), ("biomass", "biomasa"), ("coal", "carbón"), ("H2", "hidrógeno"),
            ("industr", "industria"), ("process heat", "industria"), ("iron", "industria"),
            ("cement", "industria"), ("chemical", "industria"), ("paper", "industria"),
            ("alumin", "industria"), ("food", "industria")]


def family_of(sector):
    s = re.sub(r"\s*(EUR)?_d\d+$", "", sector)
    s = re.sub(r"\s+EUR$", "", s)
    for k, v in FAMILIES:
        if k in s:
            return v
    return s


def module_name(members):
    regs = pd.Series([m.split("|", 1)[0] for m in members]).value_counts()
    fams = pd.Series([family_of(m.split("|", 1)[1]) for m in members]).value_counts()
    reg = regs.index[0] if regs.iloc[0] / len(members) >= 0.6 else f"{len(regs)} regiones"
    return f"{reg} · {' / '.join(fams.index[:2])}"


# ---------------------------------------------------------------- datos
def load_data(base_dir=BASE_DIR):
    nodes = pd.read_csv(os.path.join(base_dir, "gcam_graph_nodes.csv"), low_memory=False)
    sector_edges = pd.read_csv(os.path.join(base_dir, "gcam_graph_sector_edges.csv"), low_memory=False)

    mk = nodes[nodes.node_type == "market"].set_index("node_id")
    t = nodes[(nodes.node_type == "technology") & (nodes.output_kind == "primary")]
    sn = (t.groupby(["region", "sector"])
           .agg(q_ref=("value_ref", "sum"), q_chY=("value_chY", "sum"), mkt=("sells_to_market", "first"))
           .reset_index())
    sn["name"] = sn.region + "|" + sn.sector
    sn["e_node"] = lnr(sn.q_chY, sn.q_ref)
    sn["e_market"] = lnr(sn.mkt.map(mk.supply_chY), sn.mkt.map(mk.supply_ref))
    sn["e_local"] = sn.e_node - sn.e_market
    sn["share_inherited"] = np.abs(sn.e_market) / (np.abs(sn.e_market) + np.abs(sn.e_local))
    return sn.set_index("name"), sector_edges


# ---------------------------------------------------------------- generador
def graph_to_mermaid(sec_nodes, sector_edges, seed, order=4, mode="all", only_europe=True,
                     exclude=r"water|desalinated|UnmanagedLand", resolution=1.0, max_members=10,
                     small_module=3, expand=None, trace_min_share=0.25, trace_depth=8,
                     direction="LR", out=None, seed_rng=1):
    expand = set(expand or [])
    e_node = sec_nodes.e_node.to_dict()
    inh = sec_nodes.share_inherited.to_dict()

    se = sector_edges[sector_edges.in_europe] if only_europe else sector_edges
    se = se[~se.from_sector.str.contains(exclude) & ~se.to_sector.str.contains(exclude)].copy()
    se["f"] = se.from_region + "|" + se.from_sector
    se["t"] = se.to_region + "|" + se.to_sector
    se["e_flow"] = lnr(se.flow_chY, se.flow_ref)

    G = nx.DiGraph()
    G.add_edges_from(zip(se.f, se.t))
    if seed not in G:
        raise ValueError(f"Semilla no encontrada: {seed}")

    # vecindario (mode: all / out / in, igual que igraph::ego)
    H = G.to_undirected(as_view=True) if mode == "all" else (G if mode == "out" else G.reverse(copy=False))
    keep = set(nx.single_source_shortest_path_length(H, seed, cutoff=order))
    E = se[se.f.isin(keep) & se.t.isin(keep)].reset_index(drop=True)
    gs = G.subgraph(keep)

    # módulos
    comms = nx.community.louvain_communities(gs.to_undirected(), resolution=resolution, seed=seed_rng)
    comms = sorted([sorted(c) for c in comms], key=len, reverse=True)

    # traza del error heredado
    trace = []
    cur, seen = seed, {seed}
    for _ in range(trace_depth):
        d = E[E.f == cur]
        if d.empty:
            break
        LS = lmean(d.flow_chY.sum(), d.flow_ref.sum())
        contrib = (d.flow_chY.fillna(0) - d.flow_ref.fillna(0)) / LS
        tot = contrib.sum()
        if not np.isfinite(tot) or abs(tot) < 1e-9:
            break
        sh = contrib / tot
        j = sh.idxmax()
        if sh[j] < trace_min_share or d.t[j] in seen:
            break
        trace.append((cur, d.t[j], float(sh[j])))
        seen.add(d.t[j]); cur = d.t[j]
    trace_nodes = {seed} | {a for a, _, _ in trace} | {b for _, b, _ in trace}

    def hot(v):
        x = e_node.get(v, np.nan)
        return abs(pct(x)) if np.isfinite(x) else -1.0

    node_id = {v: f"N{i}" for i, v in enumerate(sorted(keep), 1)}
    disp, groups = {}, []
    for i, m in enumerate(comms, 1):
        gid = f"M{i}"
        exp_it = bool(set(m) & trace_nodes) or len(m) <= small_module or bool(set(m) & expand)
        gr = dict(id=gid, members=m, expand=exp_it, name=module_name(m))
        if exp_it:
            must = [v for v in m if v in trace_nodes or v in expand]
            rest = sorted([v for v in m if v not in must], key=hot, reverse=True)
            shown = must + rest[:max(0, max_members - len(must))]
            gr["shown"], gr["others"] = shown, [v for v in m if v not in shown]
            for v in shown:
                disp[v] = node_id[v]
            for v in gr["others"]:
                disp[v] = gid + "x"
        else:
            for v in m:
                disp[v] = gid
        groups.append(gr)

    def med_abs(vs):
        x = np.array([e_node.get(v, np.nan) for v in vs], float)
        x = x[np.isfinite(x)]
        return float(np.median(np.abs(pct(x)))) if len(x) else np.nan

    fmt = lambda x, f: f.format(x) if np.isfinite(x) else "s/d"

    L = [f"flowchart {direction}"]
    for gr in groups:
        if gr["expand"]:
            L.append(f'  subgraph {gr["id"]}["{esc(gr["name"])} ({len(gr["members"])})"]')
            for v in gr["shown"]:
                reg, sec = v.split("|", 1)
                e = e_node.get(v, np.nan); p = pct(e) if np.isfinite(e) else np.nan
                h = inh.get(v, np.nan)
                val = "s/d" if not np.isfinite(p) else f"<b>{p:+.1f}%</b> · her. {fmt(100 * h, '{:.0f}%')}"
                txt = f"{esc(sec)}<br/><small>{reg}</small><br/>{val}"
                o, c = ('{{"', '"}}') if v == seed else ('["', '"]')
                L.append(f"    {node_id[v]}{o}{txt}{c}:::{heat_class(p)}")
            if gr["others"]:
                om = med_abs(gr["others"])
                L.append(f'    {gr["id"]}x(["+{len(gr["others"])} sectores<br/>mediana {fmt(om, "{:.1f}%")}"]):::{heat_class(om)}')
            L.append("  end")
        else:
            med = med_abs(gr["members"])
            top = max(gr["members"], key=hot)
            L.append(f'  {gr["id"]}[["<b>{esc(gr["name"])}</b><br/>{len(gr["members"])} sectores<br/>'
                     f'mediana {fmt(med, "{:.1f}%")} · máx {fmt(hot(top), "{:.0f}%")}<br/>'
                     f'<small>{esc(top.split("|", 1)[1])}</small>"]]:::{heat_class(med)}')

    # aristas agregadas
    tr = {}
    for a, b, s in trace:
        k = (disp[a], disp[b]); tr[k] = max(tr.get(k, -np.inf), s)
    E2 = E.assign(a=E.f.map(disp), b=E.t.map(disp))
    E2 = E2[E2.a != E2.b]
    agg = (E2.groupby(["a", "b"])
             .agg(n=("e_flow", "size"), e=("e_flow", "median"))
             .reset_index())
    styles = []
    for k, r in enumerate(agg.itertuples(index=False)):
        p = pct(r.e) if np.isfinite(r.e) else np.nan
        lab = f"{p:+.1f}%" if np.isfinite(p) else ""
        share = tr.get((r.a, r.b))
        if share is not None:
            lab = f"{lab} · aporta {100 * share:.0f}%"
        if r.n > 1:
            lab += f" ({r.n})"
        L.append(f'  {r.a} -->|"{lab}"| {r.b}' if lab else f"  {r.a} --> {r.b}")
        if share is not None:
            col, w = "#ff8c00", 5
        elif not np.isfinite(p):
            col, w = "#777777", 1
        else:
            a = abs(p); i = int(np.searchsorted([5, 20], a, side="right"))
            w = [1, 2, 3.5][i]
            col = (["#f4a582", "#d6604d", "#b2182b"] if p > 0 else ["#92c5de", "#4393c3", "#2166ac"])[i]
        styles.append(f"  linkStyle {k} stroke:{col},stroke-width:{w}px")
    L += styles
    pal = ["#1b0c41", "#4a0c6b", "#a52c60", "#ed6925", "#fbb61a", "#fcffa4"]
    for i, c in enumerate(pal):
        L.append(f"  classDef h{i} fill:{c},stroke:#222,color:{'#fff' if i < 3 else '#000'}")
    L.append("  classDef hna fill:#bbbbbb,stroke:#666,color:#000")
    txt = "\n".join(L)

    if out:
        with open(out + ".mmd", "w", encoding="utf-8") as fh:
            fh.write(txt)
        write_mermaid_html(txt, out + ".html", f"Error hindcast 2021 · {seed} · orden {order}")
    print(f"{len(keep)} sectores en el vecindario -> {len(groups)} módulos, {len(agg)} aristas; "
          f"traza: {len(trace)} pasos")
    return dict(mermaid=txt, modules=comms, trace=trace, edges=agg)


def write_mermaid_html(txt, path, title="GCAM error graph"):
    html = f"""<!doctype html><html><head><meta charset="utf-8"><title>{title}</title>
<style>body{{margin:0;font-family:sans-serif;background:#fafafa}} #out{{padding:16px}} h3{{margin:12px 16px}}
.legend{{margin:0 16px;font-size:13px;color:#444}}</style></head><body>
<h3>{title}</h3>
<div class="legend">Color del nodo = |error| (oscuro &lt;2% · 2–5 · 5–10 · 10–20 · 20–40 · claro &gt;40%).
 Aristas: rojo = sobreestima, azul = subestima, grosor = |error| del flujo.
 Naranja = traza del error heredado. Cajas dobles = módulos plegados.</div>
<div id="out">Cargando…</div>
<script type="module">
import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11.4.1/dist/mermaid.esm.min.mjs';
mermaid.initialize({{startOnLoad:false, maxTextSize:500000, maxEdges:3000, securityLevel:'loose',
  flowchart:{{htmlLabels:true, curve:'basis'}}}});
const def = {json.dumps(txt)};
const {{svg}} = await mermaid.render('g', def);
document.getElementById('out').innerHTML = svg;
</script></body></html>"""
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(html)


# ---------------------------------------------------------------- CLI
if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("seed", help='nodo semilla "región|sector", p. ej. "Iberian_Peninsula|electricity"')
    ap.add_argument("--order", type=int, default=4)
    ap.add_argument("--mode", choices=["all", "out", "in"], default="all")
    ap.add_argument("--all-regions", action="store_true", help="no limitar a regiones europeas")
    ap.add_argument("--exclude", default=r"water|desalinated|UnmanagedLand")
    ap.add_argument("--resolution", type=float, default=1.0, help=">1 = módulos más pequeños")
    ap.add_argument("--max-members", type=int, default=10)
    ap.add_argument("--small-module", type=int, default=3)
    ap.add_argument("--expand", action="append", default=[], help='nodo cuyo módulo desplegar (repetible)')
    ap.add_argument("--direction", default="LR", choices=["LR", "TD", "RL", "BT"])
    ap.add_argument("--base-dir", default=BASE_DIR)
    ap.add_argument("--out", default=None, help="ruta sin extensión (por defecto, en base-dir)")
    a = ap.parse_args()

    sec_nodes, sector_edges = load_data(a.base_dir)
    out = a.out or os.path.join(a.base_dir, "mermaid_" + re.sub(r"[^A-Za-z0-9]+", "_", a.seed).strip("_")
                                + f"_o{a.order}")
    res = graph_to_mermaid(sec_nodes, sector_edges, a.seed, order=a.order, mode=a.mode,
                           only_europe=not a.all_regions, exclude=a.exclude, resolution=a.resolution,
                           max_members=a.max_members, small_module=a.small_module, expand=a.expand,
                           direction=a.direction, out=out)
    print("Escrito:", out + ".mmd", "|", out + ".html")
    for a_, b_, s_ in res["trace"]:
        print(f"  traza: {a_} -> {b_}  (aporta {100 * s_:.0f}%)")
