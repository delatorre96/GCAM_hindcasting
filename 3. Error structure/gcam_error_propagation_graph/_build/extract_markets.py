"""Extrae de los XML de GCAM-Europe la región de mercado de cada insumo (<market-name>)
y de cada sector/recurso (<market>), en el orden de carga de la configuración.
Uso: python extract_markets.py <gcam-europe/exe/configuration.xml> <carpeta_salida>"""
import sys, os, re, csv, xml.etree.ElementTree as ET
cfg, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)
exe = os.path.dirname(os.path.abspath(cfg))
txt = re.sub(r'<!--.*?-->', '', open(cfg, encoding='utf-8').read(), flags=re.S)
files = re.findall(r'>\s*([^<>]*gcamdata/xml/[^<>]*\.xml)\s*<', txt)
skip = re.compile(r'emissions|prodchange|ag_cost|resbio|land_input|HDDCDD|hector|modeltime|socioeconomics|Ccoef|prune|MAC|protected')
with open(os.path.join(out, 'cfg_order.tsv'), 'w') as f:
    for k, p in enumerate(files, 1):
        f.write(f"{k}\t{os.path.basename(p)}\n")
SECT = ('supplysector', 'AgSupplySector', 'pass-through-sector', 'energy-final-demand', 'resource',
        'renewresource', 'unlimited-resource', 'gcam-consumer')
SUBS = ('subsector', 'AgSupplySubsector', 'tranSubsector', 'nesting-subsector', 'subresource')
inp = set(); sup = set()
for p in files:
    if skip.search(p):
        continue
    path = os.path.normpath(os.path.join(exe, p)); base = os.path.basename(path)
    stack = []
    for ev, el in ET.iterparse(path, events=('start', 'end')):
        if ev == 'start':
            stack.append((el.tag, el.get('name'))); continue
        if el.tag in ('market-name', 'internal-gains-market-name', 'marginal-revenue-market'):
            ctx = {}
            for t, n in stack[:-1]:
                if t == 'region': ctx['region'] = n
                elif t in SECT: ctx['sector'] = n; ctx['tag'] = t
                elif t in SUBS: ctx['subsector'] = n
                elif 'technology' in t or t == 'AgProductionTechnology': ctx['technology'] = n
                elif t == 'global-technology-database': ctx['region'] = 'GLOBAL_TECH_DB'
                elif t == 'location-info': ctx['region'] = 'LOCINFO'
            par = stack[-2]
            inp.add((base, ctx.get('region'), ctx.get('tag'), ctx.get('sector'), ctx.get('subsector'),
                     ctx.get('technology'), par[0], par[1], el.tag, (el.text or '').strip()))
        elif el.tag == 'market':
            reg = next((n for t, n in stack if t == 'region'), None)
            par = stack[-2]
            sup.add((base, reg, par[0], par[1], (el.text or '').strip()))
        stack.pop()
        if len(stack) <= 3: el.clear()
    print('ok', base)
with open(os.path.join(out, 'markets_dedup.tsv'), 'w', newline='') as f:
    w = csv.writer(f, delimiter='\t')
    w.writerow(['file', 'region', 'sector_tag', 'sector', 'subsector', 'technology', 'elem_tag', 'elem_name',
                'market_tag', 'market'])
    for r in sorted(inp, key=lambda r: tuple('' if x is None else x for x in r)): w.writerow(r)
with open(os.path.join(out, 'supply_markets.tsv'), 'w', newline='') as f:
    w = csv.writer(f, delimiter='\t')
    for r in sorted(sup, key=lambda r: tuple('' if x is None else x for x in r)): w.writerow(r)
