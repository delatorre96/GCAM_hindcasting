"""Convierte un proyecto rgcam (.dat) en pickles <escenario>__<query>.pkl sin necesitar R.
Uso: python extract_dat.py <proyecto.dat> <carpeta_salida>"""
import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rparse
src, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)
o = rparse.load(src)
tag, prj = o.value[0]
for scen, sv in zip(prj.attrs['names'].value, prj.value):
    for q, qv in zip(sv.attrs['names'].value, sv.value):
        df = rparse.to_df(qv)
        df.to_pickle(os.path.join(out, f"{scen}__{q.replace(' ', '_')}.pkl"))
        print(scen, q, df.shape)
