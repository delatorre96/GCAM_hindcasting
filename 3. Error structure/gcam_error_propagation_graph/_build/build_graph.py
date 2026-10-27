import pandas as pd, numpy as np, os

# Uso:  python build_graph.py <carpeta_pkl> <carpeta_build> <carpeta_salida>
#   carpeta_pkl   : pickles creados por extract_dat.py a partir del .dat de rgcam
#   carpeta_build : markets_dedup.tsv, supply_markets.tsv, cfg_order.tsv (de extract_markets.py)
import sys
SP, BUILD, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
os.makedirs(OUT, exist_ok=True)
YEAR = 2021
REF, CHY = "Reference", "BaseYear2015_policy_test_noEEAtradetax"
KT = ["region", "sector", "subsector", "technology"]

EUR = ["Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France",
       "Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland",
       "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Albania","Bosnia and Herzegovina","Iceland",
       "Macedonia","Moldova","Norway","Serbia and Montenegro","Turkey","UK","Ukraine","Appenine_Peninsula",
       "British_Isles","Central_Eastern_Europe","Central_Western_Europe","Iberian_Peninsula","Northern_Europe",
       "South_Eastern_Europe","Ukraine_Moldova","European_Single_Market"]


def load(scen, q, valname):
    d = pd.read_pickle(f"{SP}/{scen}__{q}.pkl")
    d = d[d.year == YEAR].drop(columns=["scenario", "year"])
    return d.rename(columns={"value": valname})


# ---------- technology outputs (nodes) ----------
ko = KT + ["output"]
o = load(REF, "outputs_by_tech", "value_ref").merge(
    load(CHY, "outputs_by_tech", "value_chY"), on=ko + ["Units"], how="outer")
o = o.groupby(ko, as_index=False).agg(Units=("Units", "first"), value_ref=("value_ref", "sum"),
                                       value_chY=("value_chY", "sum"))
# ---------- technology inputs (consumption edges) ----------
ki = KT + ["input"]
i = load(REF, "inputs_by_tech", "flow_ref").merge(
    load(CHY, "inputs_by_tech", "flow_chY"), on=ki + ["Units"], how="outer")
i = i.groupby(ki, as_index=False).agg(Units=("Units", "first"), flow_ref=("flow_ref", "sum"),
                                       flow_chY=("flow_chY", "sum"))

# ---------- market names from XML (last loaded file wins) ----------
m = pd.read_csv(f"{BUILD}/markets_dedup.tsv", sep="\t", dtype=str)
order = pd.read_csv(f"{BUILD}/cfg_order.tsv", sep="\t", header=None, names=["ord", "file"])
order = order.groupby("file").ord.max()
m["ord"] = m.file.map(order).fillna(-1)
mi = m[(m.market_tag == "market-name") & m.elem_tag.isin(
    ["minicam-energy-input", "building-node-input", "input-accounting"])].copy()
mi = mi.sort_values("ord")
mi = mi.rename(columns={"elem_name": "input"})
exact = mi.drop_duplicates(KT + ["input"], keep="last").set_index(KT + ["input"]).market
lvl_sector = mi.drop_duplicates(["region", "sector", "input"], keep="last").set_index(["region", "sector", "input"]).market
reg_markets = mi.groupby(["region", "input"]).market.agg(lambda s: s.iloc[-1] if s.nunique() == 1 else None)
gdb = mi[mi.region == "GLOBAL_TECH_DB"]

sm = pd.read_csv(f"{BUILD}/supply_markets.tsv", sep="\t", header=None, dtype=str,
                 names=["file", "region", "tag", "name", "market"])
sm["ord"] = sm.file.map(order).fillna(-1)
sm = sm[sm.tag != "policy-portfolio-standard"].sort_values("ord").drop_duplicates(["region", "name"], keep="last")
supply_market = sm.set_index(["region", "name"]).market.to_dict()   # (region, sector/resource) -> market region
o["market_region"] = [supply_market.get((r, sec), r) if out == sec else r
                      for r, sec, out in zip(o.region, o.sector, o.output)]
sector_regions = o.groupby("sector").region.agg(set).to_dict()
sector_set = set(zip(o.region, o.sector))

mk, src = [], []
for r, s, ss, t, inp in i[ki].itertuples(index=False):
    k = (r, s, ss, t, inp)
    if k in exact.index:
        mk.append(exact[k]); src.append("xml_technology")
    elif (r, s, inp) in lvl_sector.index:
        mk.append(lvl_sector[(r, s, inp)]); src.append("xml_sector")
    elif (r, inp) in reg_markets.index and pd.notna(reg_markets[(r, inp)]):
        mk.append(reg_markets[(r, inp)]); src.append("xml_region")
    elif (r, inp) in supply_market:
        mk.append(supply_market[(r, inp)]); src.append("xml_supply_market")
    elif (r, inp) in sector_set:
        mk.append(r); src.append("same_region_sector")
    else:
        regs = sector_regions.get(inp)
        if regs and len(regs) == 1:
            mk.append(next(iter(regs))); src.append("unique_producer_region")
        else:
            mk.append(r); src.append("default_own_region")
i["market_region"], i["market_source"] = mk, src
_sold = set(zip(o.market_region, o.output))
i["supplied_by_sector"] = [(mr, g) in _sold for mr, g in zip(i.market_region, i.input)]

# ---------- node tables ----------
o["node_id"] = o.region + "|" + o.sector + "|" + o.subsector + "|" + o.technology + "|" + o.output
o["market_id"] = "MKT|" + o.market_region + "|" + o.output  # where this output is sold
# secondary-output market overrides
so = m[(m.elem_tag == "secondary-output") & (m.market_tag == "market-name")].sort_values("ord")
so = so.drop_duplicates(KT + ["elem_name"], keep="last").set_index(KT + ["elem_name"]).market
for idx, row in o[o.output != o.sector].iterrows():
    k = (row.region, row.sector, row.subsector, row.technology, row.output)
    if k in so.index:
        o.at[idx, "market_id"] = "MKT|" + so[k] + "|" + row.output
o["output_kind"] = np.where(o.output == o.sector, "primary", "secondary")

i["market_id"] = "MKT|" + i.market_region + "|" + i.input
i["consumer_tech_id"] = i.region + "|" + i.sector + "|" + i.subsector + "|" + i.technology

# a consuming technology is identified by region|sector|subsector|technology; its primary node adds |output
prim = o[o.output_kind == "primary"][KT + ["node_id"]]
tech_map = o.assign(tech_id=o.region + "|" + o.sector + "|" + o.subsector + "|" + o.technology)

tech_nodes = o.assign(node_type="technology")[[
    "node_id", "node_type", "region", "sector", "subsector", "technology", "output", "output_kind", "Units",
    "value_ref", "value_chY", "market_id"]]

all_mkt = pd.Index(sorted(set(o.market_id.dropna()) | set(i.market_id.dropna())))
supply = o.groupby("market_id").agg(supply_ref=("value_ref", "sum"), supply_chY=("value_chY", "sum"),
                                    Units=("Units", "first"), n_suppliers=("node_id", "size"))
demand = i.groupby("market_id").agg(demand_ref=("flow_ref", "sum"), demand_chY=("flow_chY", "sum"),
                                    Units_d=("Units", "first"), n_consumers=("consumer_tech_id", "size"))
mk_nodes = pd.DataFrame({"node_id": all_mkt})
mk_nodes["region"] = mk_nodes.node_id.str.split("|").str[1]
mk_nodes["good"] = mk_nodes.node_id.str.split("|", n=2).str[2]
mk_nodes = mk_nodes.join(supply, on="node_id").join(demand, on="node_id")
mk_nodes["Units"] = mk_nodes.Units.fillna(mk_nodes.Units_d)
mk_nodes = mk_nodes.drop(columns="Units_d")
mk_nodes["node_type"] = np.where(mk_nodes.n_suppliers.fillna(0) > 0, "market", "exogenous_or_resource")

# ---------- edges ----------
# (1) technology -> market (supplies)
e_sup = o.assign(edge_type="supplies", good=o.output)[["node_id", "market_id", "edge_type", "good", "Units",
                                                        "value_ref", "value_chY"]]
e_sup.columns = ["from", "to", "edge_type", "good", "Units", "flow_ref", "flow_chY"]
e_sup["market_source"] = "output"
# (2) market -> consuming technology node(s). Attach to every output node of that technology
#     (normally just the primary one).
ii = i.merge(tech_map[["tech_id", "node_id", "output_kind"]], left_on="consumer_tech_id", right_on="tech_id", how="left")
# prefer primary node; if a tech has no output row in 2021, keep a tech-only id
ii = ii.sort_values("output_kind").drop_duplicates(ki + ["market_id"], keep="first")
missing_out = ii.node_id.isna()
ii.loc[missing_out, "node_id"] = ii.loc[missing_out, "consumer_tech_id"] + "|<no output in 2021>"
e_con = ii.assign(edge_type="consumes", good=ii.input)[["market_id", "node_id", "edge_type", "good", "Units",
                                                         "flow_ref", "flow_chY", "market_source"]]
e_con.columns = ["from", "to", "edge_type", "good", "Units", "flow_ref", "flow_chY", "market_source"]
edges = pd.concat([e_sup, e_con], ignore_index=True)

# nodes that only consume (no output in 2021)
extra = ii[missing_out][["node_id", "region", "sector", "subsector", "technology"]].drop_duplicates("node_id")
extra = extra.assign(node_type="technology", output=None, output_kind="none")
nodes = pd.concat([
    tech_nodes,
    extra,
    mk_nodes.rename(columns={"good": "output"}).assign(sector=lambda d: d.output, technology=None, subsector=None,
                                                         output_kind="market")
], ignore_index=True)
nodes = nodes.rename(columns={"market_id": "sells_to_market"})
nodes["in_europe"] = nodes.region.isin(EUR)
cols = ["node_id", "node_type", "region", "sector", "subsector", "technology", "output", "output_kind",
        "in_europe", "Units", "value_ref", "value_chY", "sells_to_market", "supply_ref", "supply_chY",
        "demand_ref", "demand_chY", "n_suppliers", "n_consumers"]
nodes = nodes[cols]
edges["from_region"] = edges["from"].str.replace("^MKT\\|", "", regex=True).str.split("|").str[0]
edges["to_region"] = edges["to"].str.replace("^MKT\\|", "", regex=True).str.split("|").str[0]
edges["cross_region"] = edges.from_region != edges.to_region

# ---------- sector-level graph (supplier region/sector -> consumer region/sector) ----------
sup = o[["region", "sector", "market_id", "value_ref"]].rename(
    columns={"region": "from_region", "sector": "from_sector"})
supw = sup.groupby(["market_id", "from_region", "from_sector"], as_index=False).value_ref.sum()
supw["w"] = supw.value_ref / supw.groupby("market_id").value_ref.transform("sum")
con = i.groupby(["market_id", "input", "region", "sector"], as_index=False).agg(
    flow_ref=("flow_ref", "sum"), flow_chY=("flow_chY", "sum"), Units=("Units", "first"))
sec = con.merge(supw[["market_id", "from_region", "from_sector", "w"]], on="market_id", how="left")
sec["from_region"] = sec.from_region.fillna(sec.market_id.str.split("|").str[1])
sec["from_sector"] = sec.from_sector.fillna("[exogenous] " + sec.input)
sec["w"] = sec.w.fillna(1.0)
sec["flow_ref"] *= sec.w
sec["flow_chY"] *= sec.w
sector_edges = sec.groupby(["from_region", "from_sector", "input", "region", "sector", "Units"], as_index=False).agg(
    flow_ref=("flow_ref", "sum"), flow_chY=("flow_chY", "sum"))
sector_edges = sector_edges.rename(columns={"input": "good", "region": "to_region", "sector": "to_sector"})
sector_edges["in_europe"] = sector_edges.to_region.isin(EUR) | sector_edges.from_region.isin(EUR)

nodes.to_csv(f"{OUT}/gcam_graph_nodes.csv", index=False)
edges.to_csv(f"{OUT}/gcam_graph_edges.csv", index=False)
sector_edges.to_csv(f"{OUT}/gcam_graph_sector_edges.csv", index=False)
print(nodes.node_type.value_counts())
print(edges.edge_type.value_counts())
print("sector edges", len(sector_edges))
print(i.market_source.value_counts())
print("consumes edges whose market has a producing sector:", i.supplied_by_sector.mean())
print("cross-region consumes:", (i.market_region != i.region).mean())

# ---------- close market balances: unrecorded final demand (food demand, policy markets, stock changes) ----------
mkn = nodes[nodes.node_type == "market"].copy()
res_ref = (mkn.supply_ref.fillna(0) - mkn.demand_ref.fillna(0))
res_chY = (mkn.supply_chY.fillna(0) - mkn.demand_chY.fillna(0))
tol = 0.01 * mkn.supply_ref.abs().fillna(0)
sel = (res_ref.abs() > 1e-12) | (res_chY.abs() > 1e-12)
sink = pd.DataFrame({"from": mkn.node_id[sel], "to": "SINK|" + mkn.node_id[sel].str[4:],
                     "edge_type": "unrecorded_final_demand",
                     "good": mkn.sector[sel], "Units": mkn.Units[sel],
                     "flow_ref": res_ref[sel], "flow_chY": res_chY[sel],
                     "market_source": "residual_supply_minus_recorded_demand"})
sink["from_region"] = mkn.region[sel]; sink["to_region"] = mkn.region[sel]; sink["cross_region"] = False
sink_nodes = pd.DataFrame({"node_id": sink.to, "node_type": "final_demand_residual", "region": sink.from_region,
                           "sector": sink.good, "output": sink.good, "output_kind": "sink",
                           "in_europe": sink.from_region.isin(EUR), "Units": sink.Units,
                           "value_ref": sink.flow_ref, "value_chY": sink.flow_chY})
nodes = pd.concat([nodes, sink_nodes], ignore_index=True)
edges = pd.concat([edges, sink], ignore_index=True)
nodes.to_csv(f"{OUT}/gcam_graph_nodes.csv", index=False)
edges.to_csv(f"{OUT}/gcam_graph_edges.csv", index=False)
print("residual sink edges:", len(sink))

# ---------- feedback loops (strongly connected components) ----------
import networkx as nx
G = nx.DiGraph()
fe = edges[edges.edge_type != "unrecorded_final_demand"]
G.add_edges_from(zip(fe["from"], fe["to"]))
scc = [c for c in nx.strongly_connected_components(G) if len(c) > 1]
scc.sort(key=len, reverse=True)
rows = []
for k, c in enumerate(scc, 1):
    for v in c:
        rows.append({"loop_id": k, "loop_size": len(c), "node_id": v})
loops = pd.DataFrame(rows)
loops.to_csv(f"{OUT}/gcam_graph_feedback_loops.csv", index=False)
print("SCCs (>1 node):", len(scc), "sizes:", [len(c) for c in scc[:10]])
# sector-level loop summary for Europe
SG = nx.DiGraph()
se = sector_edges[~sector_edges.from_sector.str.startswith("[exogenous]")]
SG.add_edges_from(zip(se.from_region + "|" + se.from_sector, se.to_region + "|" + se.to_sector))
sscc = sorted([c for c in nx.strongly_connected_components(SG) if len(c) > 1], key=len, reverse=True)
print("sector-level SCCs:", len(sscc), [len(c) for c in sscc[:10]])
