# Grafo de GCAM-Europe para estudiar la propagación del error de hindcast

Año evaluado: **2021**. `value_ref` = escenario `Reference` (año base 2021); `value_chY` = `BaseYear2015_policy_test_noEEAtradetax` (año base 2015).
Fuente: `2. Extraction/BaseYear2015_outputsByTech.dat` (queries *outputs by tech* e *inputs by tech*) + `market-name` / `market` de los XML que carga `exe/configuration_eur_ref.xml` (GCAM-Europe v8.7.2, commit 0d962e4).

## Estructura del grafo

GCAM es un grafo **bipartito tecnología ↔ mercado**: las tecnologías venden a un mercado y compran a otros. Así se modeliza tal cual, porque el equilibrio se resuelve en los mercados.

```
tecnología (proveedora) --supplies--> MKT|región_mercado|bien --consumes--> tecnología (consumidora)
                                                          \--unrecorded_final_demand--> SINK|región|bien
```

### `gcam_graph_nodes.csv`

| columna | significado |
|---|---|
| `node_id` | Tecnologías: `region|sector|subsector|technology|output` (**las mismas 5 claves que *outputs by tech***). Mercados: `MKT|región_mercado|bien`. Sumideros: `SINK|región|bien`. |
| `node_type` | `technology`, `market`, `exogenous_or_resource` (recursos, agua, tierra: no aparecen en *outputs by tech*), `final_demand_residual` |
| `output_kind` | `primary` (output = sector), `secondary` (coproducto), `market`, `sink`, `none` (consume pero sin output en 2021) |
| `in_europe` | región europea, incluidas las regiones de red y `European_Single_Market` |
| `value_ref`, `value_chY` | output de la tecnología en 2021 en cada escenario |
| `sells_to_market` | mercado al que vende la tecnología |
| `supply_*`, `demand_*` | mercados: oferta total y demanda registrada |

### `gcam_graph_edges.csv`

| columna | significado |
|---|---|
| `from`, `to` | `node_id` de origen y destino |
| `edge_type` | `supplies` (tec → mercado), `consumes` (mercado → tec), `unrecorded_final_demand` (mercado → sumidero) |
| `good`, `Units` | bien intercambiado |
| `flow_ref`, `flow_chY` | flujo 2021 en cada escenario (las aristas también tienen error) |
| `market_source` | cómo se resolvió la región de mercado: `xml_technology`, `xml_sector`, `xml_region`, `xml_supply_market`, `same_region_sector`, `default_own_region` |
| `cross_region` | la arista cruza regiones (comercio, mercados comunes, red) |

### Otros ficheros

- `gcam_graph_sector_edges.csv`: grafo colapsado `región|sector → región|sector`. Cuando un mercado tiene varios proveedores, el flujo se reparte según su cuota de oferta.
- `gcam_graph_feedback_loops.csv`: componentes fuertemente conexas (bucles de retroalimentación). Hay 18. La mayor tiene **7.450 nodos**: el núcleo energía–agricultura (biocombustibles, fertilizante/amoniaco, electricidad, gas, refino, agua de riego).
- `error_propagation.R`: análisis completo (ver abajo).
- `_build/`: scripts para regenerar todo si cambias las queries o la versión del modelo.

## Validación

- Las 15.769 filas de `all_errors_output_by_tech.csv` se emparejan con un nodo. El log-ratio `ln(value_chY/value_ref)` coincide con el de tu base (diferencia < 1e-12), porque el factor de normalización se cancela.
- El 90 % de los mercados intermedios cuadra oferta y demanda con menos del 1 % de diferencia. El resto, sobre todo los mercados de alimentos, no cuadra porque la demanda final de alimentos (`FoodDemand_*`) no es una tecnología y no aparece en *inputs by tech*. Esa diferencia se modeliza explícitamente como `unrecorded_final_demand`.
- La descomposición del error es una **identidad exacta** (residuo < 1e-14) y el sistema de propagación reproduce los errores observados (residuo máximo 2.5e-6).

## Método (`error_propagation.R`)

Error: `e = ln(value_chY / value_ref)`. Ojo: tu base usa `error = value_ref − value_chY`, es decir, el signo contrario.

1. **Descomposición local / heredado** de cada tecnología *t* que vende en el mercado *M*:
   `e_t = e_market + e_region_share + e_subsector_share + e_tech_share`.
   `e_market` es el error heredado de la demanda del mercado. El resto es el error "local" de reparto (logit), que incluye la cuota regional en los mercados comunes.
2. **Atribución del error de demanda** de cada mercado a sus consumidores, de forma exacta con LMDI: `d ln S_M = Σ_c (F̂_c − F_c) / L(Ŝ_M, S_M)`.
3. **Sistema de propagación tipo Leontief**: `e = origin + B e`, luego `e = (I − B)⁻¹ origin`.
   `origin` es el error que **nace** en cada nodo, con cuatro componentes: reparto local, cambio de coeficiente de insumo, demanda final no registrada, y flujos nuevos o desaparecidos. `(I − B)⁻¹` describe **cómo se propaga**, incluidos los bucles.
   `error_origins(node_id)` devuelve qué nodos originan el error de un nodo dado.

## Limitaciones

1. **Centrales eléctricas de los países europeos.** En GCAM-Europe están dentro de un `nesting-subsector`, y el xPath de tu query (`*[@type='sector']/*[@type='subsector']/*[@type='technology']`) no baja a ese nivel. Por eso no aparecen ni en el grafo ni en `all_errors_output_by_tech.csv`: la electricidad europea solo se ve desde la red regional (`Iberian_Peninsula|base load generation|Spain base load generation…`). Para incluirlas habría que añadir una query con un nivel de subsector más (algo como `*[@type='sector']/*[@type='subsector']/*[@type='subsector']/*[@type='technology']/…`; conviene comprobar el xPath en ModelInterface) y regenerar.
2. Las tecnologías con output nulo en 2021 en ambos escenarios no aparecen. Es normal, porque no transmiten error.
3. Solo hay cantidades, no precios. La propagación capturada es la de la **cantidad** (tirón de la demanda). Los efectos vía precios quedan dentro del término "local" (reparto logit) y no se pueden separar sin una query de precios.
4. La estructura del grafo es la de la ejecución de referencia. 616 tecnologías tienen output en un solo escenario; aparecen con NA en el otro.

## Regenerar (`_build/`)

```
python extract_dat.py "<proyecto>.dat" pkl
python extract_markets.py "<gcam-europe>/exe/configuration_eur_ref.xml" build
python build_graph.py pkl build salida
```

Requiere Python 3 con pandas y networkx. Los nombres de los escenarios están definidos al principio de `build_graph.py`.
