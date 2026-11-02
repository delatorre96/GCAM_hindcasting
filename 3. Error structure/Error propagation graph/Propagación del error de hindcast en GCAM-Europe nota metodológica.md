# Propagación del error de hindcast en GCAM-Europe: nota metodológica

Oct 27, 2026 · @Nacho

## 1. Objetivo y visión general

El método responde a una pregunta: en el hindcast de GCAM-Europe para 2021, ¿qué parte del error de cada output **nace** en ese nodo y qué parte **llega heredada** de otros nodos a través de la estructura de mercados? No busca causas en sentido estadístico: descompone de forma **exacta** (sin residuo) el error observado según las identidades contables que GCAM cumple por construcción.

Se comparan dos ejecuciones del mismo modelo en el año 2021: `Reference` (año base 2021, que reproduce los datos calibrados, valor `ref`) y `BaseYear2015_policy_test_noEEAtradetax` (año base 2015, que proyecta 2021, valor `chY`). Las cantidades salen de las queries *outputs by tech* e *inputs by tech*.

El script `error_propagation.R` encadena cuatro pasos:

1. **Descomposición exacta del error de cada tecnología** (lado de la oferta): separa el error heredado de la demanda del mercado al que vende y el error local de reparto (cuotas logit).
2. **Atribución del error de demanda de cada mercado** a sus consumidores, con el método LMDI.
3. **Sistema de propagación tipo Leontief**: combina los dos pasos anteriores en un sistema lineal que separa dónde nace el error (origen) y cómo se transmite por el grafo, bucles incluidos.
4. **Bucles de retroalimentación**: identifica las componentes fuertemente conexas, donde el error circula y se amplifica.

Cada paso es una identidad algebraica comprobable numéricamente; la sección 8 recoge los residuos obtenidos.

## 2. Convención del error: el cociente logarítmico

Para toda cantidad X (output de una tecnología, flujo de una arista, oferta de un mercado) el error es:

```latex
$$
e(X) = \ln\frac{X^{chY}}{X^{ref}} = \Delta \ln X
$$
```

Un e > 0 indica que la ejecución con base 2015 sobreestima respecto a la referencia. Para leerlo como porcentaje: 100 × (exp(e) − 1). Con errores pequeños, e ≈ error relativo (e = 0,10 equivale a +10,5 %).

Se usa el logaritmo, y no la diferencia `value_ref − value_chY` de la base de errores, por cuatro razones:

- **Convierte productos en sumas.** GCAM calcula cantidades como productos (demanda × cuota × cuota …; output × coeficiente). Con logaritmos, el error de un producto es la suma exacta de los errores de sus factores. Esta propiedad sostiene los pasos 1 y 3.
- **No depende de unidades ni escala.** Permite comparar EJ, Mt y km³, y el factor de normalización de tu base de errores se cancela: el cociente logarítmico del grafo coincide con el de `all_errors_output_by_tech.csv` con una diferencia menor de 10⁻¹².
- **Es simétrico.** Duplicar (+0,69) y dividir por dos (−0,69) pesan lo mismo, cosa que no ocurre con el error relativo.
- **Es aditivo a lo largo de una cadena.** El error de un nodo se puede repartir entre los eslabones que lo forman sin términos cruzados.

Cuando una de las dos cantidades es cero (una tecnología que aparece o desaparece), el logaritmo no está definido. Esos casos se tratan aparte como "margen extensivo" (secciones 5 y 6).

Ojo con el signo: en `all_errors_output_by_tech.csv` el error es `value_ref − value_chY`, es decir, el contrario.

## 3. El grafo tecnología–mercado

El grafo reproduce cómo GCAM resuelve el equilibrio: las tecnologías no se conectan directamente entre sí, sino a través de **mercados**. Es un grafo bipartito con dos tipos de nodo y tres tipos de arista.

| Elemento | Identificador | Cantidad asociada |
| --- | --- | --- |
| Nodo tecnología | `region\|sector\|subsector\|technology\|output` (las 5 claves de *outputs by tech*) | Output Q\_t |
| Nodo mercado | `MKT\|región_mercado\|bien` | Oferta S\_M = suma de los outputs que se venden en él |
| Nodo sumidero | `SINK\|región\|bien` | Demanda final no registrada |
| Arista `supplies` | tecnología → mercado | Output vendido |
| Arista `consumes` | mercado → tecnología | Insumo comprado F (de *inputs by tech*) |
| Arista `unrecorded_final_demand` | mercado → sumidero | Oferta − demanda registrada |

&#91;embedded content: cadena tecnología–mercado · dónde nace y hacia dónde viaja el error\]

Las cantidades fluyen de izquierda a derecha, pero el error se hereda de derecha a izquierda: cada mercado transmite a sus proveedores el error de demanda de sus consumidores. Es la misma cadena del ejemplo de la sección 6.

**Región de mercado.** Un insumo no siempre se compra en la región del consumidor. GCAM-Europe tiene mercados anidados: los países compran `regional electricity` a su red (por ejemplo `Iberian_Peninsula`), los bienes `traded …` se compran en `European_Single_Market`, y los cultivos europeos se venden a un mercado común con sede en `Austria`. La región se toma de los XML que carga `configuration_eur_ref.xml`: el `<market-name>` de cada insumo (lado de la demanda) y el `<market>` de cada sector agrícola o recurso (lado de la oferta). Cuando el XML no lo indica, se usa la región del consumidor.

**Demanda final no registrada.** *Inputs by tech* solo recoge consumos de tecnologías. La demanda final de alimentos (`FoodDemand_*`), los mercados de política y otras demandas no son tecnologías, así que algunos mercados no cuadran. La diferencia entre oferta y demanda registrada se añade como una arista hacia un sumidero. Así, en todo mercado:

```latex
$$
S_M = \sum_{t \in \text{prov}(M)} Q_t = \sum_{c \in \text{cons}(M)} F_c
$$
```

donde la suma de consumidores incluye el sumidero. Esta igualdad es la que hace exactos los pasos 2 y 3.

**Validación del grafo.** Antes de añadir los sumideros, el 90 % de los mercados intermedios ya cuadraba con menos de un 1 % de diferencia, lo que confirma que la resolución de mercados es correcta. Las 15.769 filas de tu base de errores se emparejan con un nodo tecnología.

**Tecnología primaria.** Una tecnología tiene output primario cuando el output coincide con su sector. Los coproductos (por ejemplo, la electricidad de una cogeneración industrial) entran en la oferta del mercado al que venden, pero su cantidad la fija el output primario de la tecnología, no la demanda de ese mercado. Por eso las incógnitas del paso 3 son solo las tecnologías primarias.

## 4. Paso 1: descomposición exacta del error de cada tecnología

El error de una tecnología se separa exactamente en el error de la demanda total de su mercado (heredado) y el error en la cuota que esa tecnología se lleva del mercado (local).

**Motivación.** En GCAM una tecnología no decide cuánto produce: el mercado al que vende fija una demanda total y la función logit anidada la reparte entre regiones (en mercados comunes), subsectores y tecnologías. El output de una tecnología t que vende en el mercado M es, por construcción, el producto de la oferta total y de cuotas anidadas:

```latex
$$
Q_t = S_M \cdot \underbrace{\frac{R_{r}}{S_M}}_{s_{reg}} \cdot \underbrace{\frac{U_{u}}{R_{r}}}_{s_{sub}} \cdot \underbrace{\frac{Q_t}{U_{u}}}_{s_{tec}}
$$
```

donde S\_M es la oferta total del mercado, R\_r la oferta de la región y sector de t dentro de M, y U\_u la de su subsector. Es una identidad: los denominadores se cancelan.

**Fórmula.** Al aplicar el error logarítmico, el producto se convierte en suma exacta:

```latex
$$
e_t = \underbrace{\Delta\ln S_M}_{e_{market}} + \underbrace{\Delta\ln s_{reg} + \Delta\ln s_{sub} + \Delta\ln s_{tec}}_{e_{local}}
$$
```

- **e\_market (heredado):** cuánto se equivoca la demanda total del mercado. Lo comparten todos los proveedores del mercado; su origen está aguas abajo, en los consumidores (paso 2).
- **e\_region\_share:** en mercados con varias regiones proveedoras (la red ibérica, el mercado agrícola común), cuánto se equivoca la cuota de la región. Vale 0 cuando solo hay una región.
- **e\_subsector\_share y e\_tech\_share:** error en las cuotas logit dentro del sector. Aquí entran los share-weights, los exponentes logit y los precios relativos de los insumos.

En el script se calculan agrupando las aristas `supplies` por mercado (`S`), por mercado + región + sector (`R`) y por mercado + región + sector + subsector (`U`). La columna `check = e_total − e_market − e_local` debe ser 0; el residuo máximo obtenido es 1,8 × 10⁻¹⁵.

**Ejemplo.** Un mercado de electricidad con una sola región proveedora. La tecnología es gas CC dentro del subsector gas.

| Cantidad | ref | chY | Error (ln chY/ref) |
| --- | --- | --- | --- |
| Oferta del mercado S\_M | 100 | 110 | e\_market = ln(1,10) = 0,0953 |
| Subsector gas U | 60 | 70 | e\_sub = ln((70/110)/(60/100)) = 0,0588 |
| Tecnología gas CC Q\_t | 45 | 56 | e\_tec = ln((56/70)/(45/60)) = 0,0645 |
| Total |  |  | e\_t = ln(56/45) = 0,2187 |

La suma 0,0953 + 0,0588 + 0,0645 = 0,2187 cuadra exactamente. El gas CC sobreestima un 24 %. El 44 % de ese error (0,0953 / 0,2187) viene heredado de la demanda de electricidad; el 56 % nace en el reparto logit: el subsector gas gana cuota frente a otros subsectores y, dentro de él, el CC gana cuota frente a otras tecnologías.

**Lectura del "100 % heredado".** Si una tecnología es el único proveedor de su mercado, todas sus cuotas valen 1 en ambas ejecuciones y e\_local = 0 por definición. Muchos sectores de transformación y distribución están en ese caso: su error es enteramente el de su demanda.

## 5. Paso 2: atribución del error de demanda de cada mercado a sus consumidores

El error heredado e\_market del paso 1 se reparte exactamente entre los consumidores del mercado, de modo que se sabe qué consumidor lo genera.

**Motivación.** La demanda total de un mercado es la suma de lo que compra cada consumidor: S\_M = Σ\_c F\_c (incluido el sumidero de demanda final). Si la oferta está sobreestimada, es porque algún consumidor compra de más. El problema es repartir un error logarítmico de una **suma** (no de un producto) entre sus sumandos sin dejar residuo.

**La media logarítmica.** Se define, para a, b > 0:

```latex
$$
L(a,b) = \frac{a-b}{\ln a - \ln b}, \qquad L(a,a) = a
$$
```

Cumple a − b = L(a, b) · ln(a/b) exactamente, y siempre está entre a y b. Es la herramienta del método LMDI (*Logarithmic Mean Divisia Index*, Ang 2004, 2005), estándar en descomposición de indicadores energéticos.

**Fórmula.** Como Ŝ\_M − S\_M = Σ\_c (F̂\_c − F\_c), al dividir por L(Ŝ\_M, S\_M):

```latex
$$
\Delta\ln S_M = \sum_{c} \underbrace{\frac{\hat F_c - F_c}{L(\hat S_M, S_M)}}_{contrib_c} = \sum_{c} \underbrace{\frac{L(\hat F_c, F_c)}{L(\hat S_M, S_M)}}_{\alpha_c}\,\Delta\ln F_c
$$
```

- **contrib\_c** (columna `contrib`): aportación del consumidor c al error del mercado, en unidades de error logarítmico. Las aportaciones suman exactamente e\_market.
- **α\_c** (columna `alpha`): peso de c. Es aproximadamente la cuota de c en la demanda del mercado, calculada con una media logarítmica entre ambas ejecuciones. Se usa en el paso 3.
- **contrib\_share**: contrib\_c / Σ contrib. Puede superar el 100 % o ser negativo cuando unos consumidores compensan a otros.

La primera forma (diferencia de niveles entre L) funciona también cuando un flujo es cero en una ejecución o cuando el sumidero es negativo; la segunda solo cuando ambos flujos son positivos.

**Ejemplo.** Un mercado de gas con dos consumidores.

| Consumidor | F ref | F chY | Δln F | contrib | Aporta |
| --- | --- | --- | --- | --- | --- |
| Industria | 40 | 50 | +0,2231 | 10 / 102,48 = +0,0976 | 200 % |
| Edificios | 60 | 55 | −0,0870 | −5 / 102,48 = −0,0488 | −100 % |
| Mercado S\_M | 100 | 105 | +0,0488 | +0,0488 | 100 % |

Con L(105, 100) = 102,48, las aportaciones suman exactamente el error del mercado (+0,0488). La lectura: el gas está sobreestimado un 5 % porque la industria compra un 25 % de más, aunque los edificios compensan la mitad.

**Por qué no basta con ponderar por cuotas.** La alternativa intuitiva, Σ (cuota\_ref × Δln F), da 0,4 × 0,2231 + 0,6 × (−0,0870) = 0,0371, y no 0,0488: deja un 24 % sin explicar. LMDI no deja residuo y trata igual las dos ejecuciones.

El resultado se guarda en `out_market_demand_attribution.csv`: una fila por mercado y consumidor.

## 6. Paso 3: sistema de propagación tipo Leontief y origen del error

Uniendo los pasos 1 y 2 en todas las tecnologías a la vez se obtiene un sistema lineal: el error de cada tecnología es lo que nace en ella más una combinación de los errores de sus consumidores. Al resolverlo, cada error observado queda repartido entre los nodos donde nació.

**Motivación.** Los pasos 1 y 2 miran un solo eslabón. Pero el error de demanda que recibe una central de gas viene de la red eléctrica, que a su vez lo recibe de los edificios, que lo reciben de la demanda final. Hace falta encadenar todos los eslabones, incluidos los bucles, sin contar nada dos veces. Es el mismo problema que resuelve el análisis input-output de Leontief para la producción: la inversa de Leontief suma los efectos directos e indirectos a lo largo de todas las cadenas.

**Derivación.** Se parte de tres relaciones:

1. Paso 1, para una tecnología primaria t que vende en M(t): e\_t = e\_local,t + Δln S\_M(t).
2. Paso 2, para ese mercado: Δln S\_M = Σ\_k α\_k Δln F\_k + (términos de sumidero y de flujos con cero).
3. Cada consumidor k compra F\_k = a\_k · Q\_k, donde a\_k es su coeficiente de insumo (la inversa de una eficiencia). En logaritmos: Δln F\_k = e\_k + c\_k, con c\_k = Δln a\_k.

Sustituyendo 3 en 2 y 2 en 1:

```latex
$$
e_t = \underbrace{e_{local,t} + o_{M(t)}}_{origin_t} + \sum_{k} \alpha_{M(t),k}\, e_k
$$
```

con el término de origen del mercado:

```latex
$$
o_M = \underbrace{\sum_k \alpha_{M,k}\, c_k}_{o_{coef}} + \underbrace{contrib_{sumidero}}_{o_{final}} + \underbrace{\sum_{k\ \text{no endógenos}} contrib_k}_{o_{other}}
$$
```

En forma matricial, con B\_tk = α\_M(t),k (el peso del consumidor k en el mercado al que vende t):

```latex
$$
\mathbf{e} = \mathbf{origin} + B\,\mathbf{e} \quad\Longrightarrow\quad \mathbf{e} = (I - B)^{-1}\,\mathbf{origin}
$$
```

**Los cuatro tipos de origen.**

| Origen | Símbolo | Qué significa en GCAM |
| --- | --- | --- |
| Reparto local | e\_local | Error en cuotas logit: share-weights, exponentes, precios relativos |
| Coeficiente de insumo | o\_coef | Un consumidor usa más o menos insumo por unidad de output (eficiencia, cambio tecnológico) |
| Demanda final | o\_final | Error en la demanda que no procede de otra tecnología: alimentos, servicios finales, políticas. Su raíz son PIB, población y elasticidades |
| Otros | o\_other | Flujos que existen en una sola ejecución (margen extensivo) o consumidores sin output en 2021 |

**Qué dice (I − B)⁻¹.** Su desarrollo en serie es I + B + B² + B³ + …: el error que nace en j llega a t directamente (B), a través de un intermediario (B²), de dos (B³), etc. El elemento \[(I − B)⁻¹\]\_tj es el **multiplicador**: cuánto del error nacido en j acaba en t. La contribución de j al error de t es multiplicador\_tj × origin\_j, y la suma sobre todos los j reproduce exactamente e\_t.

La función `error_origins(nodo)` calcula la fila t de (I − B)⁻¹ resolviendo (I − B)ᵀ y = u\_t (sin invertir la matriz) y devuelve los nodos que más aportan, separados por tipo de origen.

**Ejemplo: una cadena de tres tecnologías.** Una tecnología de edificios H (que vende a la demanda final) consume electricidad de una central E, que consume gas de un productor G. Cada una es el único proveedor de su mercado, así que e\_local = 0 y todos los α valen 1.

| Nodo | ref | chY | Error e | Origen que nace en su mercado |
| --- | --- | --- | --- | --- |
| H: servicio de edificios | 100 | 110 | ln(1,10) = 0,0953 | o\_final = 0,0953 (la demanda final sobreestima un 10 %) |
| E: electricidad (H usa 0,50 → 0,52 por unidad) | 50 | 57,2 | 0,1345 | o\_coef = ln(0,52/0,50) = +0,0392 (H es menos eficiente) |
| G: gas (E usa 2,0 → 1,9 por unidad) | 100 | 108,68 | 0,0832 | o\_coef = ln(1,9/2,0) = −0,0513 (E es más eficiente) |

Con B\_EH = B\_GE = 1, el sistema da e\_H = 0,0953; e\_E = 0,0392 + e\_H = 0,1345; e\_G = −0,0513 + e\_E = 0,0832. Los tres cuadran con lo observado. El error del gas (+8,7 %) se descompone así:

- **+115 %** viene de la demanda final de servicios de edificios (0,0953 / 0,0832).
- **+47 %** viene de que los edificios usan más electricidad por unidad.
- **−62 %** lo compensa la mejora de eficiencia de la central.

Sin el sistema solo veríamos un +8,7 % en el gas y lo atribuiríamos al sector gas. El sistema muestra que el gas no tiene error propio: es el resultado neto de tres errores nacidos aguas abajo.

**Detalles de implementación.**

- Las incógnitas son las tecnologías primarias con error definido (61.820 nodos). B es dispersa (341.338 elementos no nulos) y se resuelve con `Matrix`.
- Un consumidor es endógeno si es una tecnología primaria con error definido y su flujo es positivo en ambas ejecuciones. Si no, su aportación entera va a o\_other.
- El origen o\_M se asigna a cada proveedor del mercado M, porque todos lo heredan por igual. Para un total por mercado no hay que sumar los orígenes de varios proveedores del mismo mercado: se contaría el mismo término varias veces.
- o\_coef se contabiliza en el proveedor, aunque conceptualmente es un cambio en el consumidor k. `error_origins()` lo muestra en la columna `contrib_coef` del proveedor.
- Comprobación: e − (origin + B e) tiene un residuo máximo de 2,5 × 10⁻⁶ y percentil 99 de 10⁻¹³.

## 7. Paso 4: bucles de retroalimentación

El grafo tiene 18 bucles; uno de ellos reúne 7.450 nodos y es el núcleo energía–agricultura (biocombustibles, amoniaco y fertilizante, electricidad, gas, refino, agua de riego). Dentro de un bucle el error no solo se transmite: vuelve sobre sí mismo y se amplifica.

**Definición.** Un bucle es una componente fuertemente conexa (SCC): un conjunto de nodos en el que desde cualquiera se puede llegar a cualquier otro siguiendo las aristas. Por ejemplo, la electricidad se usa en refino y los líquidos refinados se usan en centrales eléctricas. Se calculan con el algoritmo de Tarjan (1972) sobre el grafo tecnología–mercado, sin los sumideros (que no tienen salida). El resultado está en `gcam_graph_feedback_loops.csv`: `loop_id`, `loop_size` y los nodos de cada bucle.

**Por qué importan.** En un grafo sin bucles, la serie I + B + B² + … termina tras un número finito de pasos. Con bucles, el error vuelve al punto de partida una y otra vez, y la serie suma todas esas vueltas. Por eso el sistema del paso 3 se resuelve con (I − B)⁻¹ y no recorriendo cadenas: es la única forma de contar bien el error que circula.

**Ejemplo: amplificación en un bucle de dos nodos.** Supongamos que el 30 % de la demanda de electricidad viene del refino (B\_EL,REF = 0,3) y el 20 % de la demanda de líquidos viene de las centrales (B\_REF,EL = 0,2):

```latex
$$
B = \begin{pmatrix} 0 & 0.3 \\ 0.2 & 0 \end{pmatrix}, \qquad (I-B)^{-1} = \frac{1}{1 - 0.3 \cdot 0.2}\begin{pmatrix} 1 & 0.3 \\ 0.2 & 1 \end{pmatrix}
$$
```

El factor 1 / (1 − 0,06) = 1,064 significa que un error nacido en la electricidad vuelve a ella amplificado un 6,4 % después de pasar por el refino. Con pesos mayores la amplificación crece rápidamente: con 0,6 y 0,5 sería 1,43.

**Condición de estabilidad.** (I − B)⁻¹ existe y la serie converge si el radio espectral de B es menor que 1, es decir, si en ningún bucle el error se multiplica en cada vuelta. En tus datos la suma por filas de B llega a 1,12 en algunos nodos, pero el sistema se resuelve sin problemas y reproduce los errores observados, lo que indica que los bucles amortiguan.

**Uso en el script.** El `loop_id` de cada tecnología se añade a `out_error_origins_EUR.csv`. Así se puede comprobar si los nodos con mayor error están dentro del gran bucle, donde el error tiene más caminos para amplificarse, y `loop_summary` lista los sectores de cada bucle.

## 8. Resúmenes, salidas y validación numérica

Todas las identidades se cumplen con precisión de máquina, así que los resultados son descomposiciones del error observado y no estimaciones.

**Resúmenes por sector (Europa).** Dentro de cada sector, los errores de sus tecnologías se promedian en valor absoluto ponderando por el output de referencia. Se pondera solo dentro del sector porque entre sectores las unidades son distintas.

- `share_local` = |e\_local| / (|e\_local| + |e\_market|): qué parte del error nace en el reparto del propio nodo.
- `born_here_ratio` = media de |origin| / media de |e|. Si es menor que 1, el sector recibe más error del que genera; si es mayor que 1, genera error que en parte se compensa en él.
- `origin_type`: el componente de origen de mayor valor absoluto en cada nodo (reparto local, coeficiente, demanda final u otros).

**Ficheros que escribe el script.**

| Fichero | Contenido |
| --- | --- |
| `out_decomposition_by_node.csv` | Paso 1 para cada tecnología: e\_total, e\_market, las tres cuotas y share\_local |
| `out_decomposition_by_sector_EUR.csv` | Paso 1 resumido por sector en Europa |
| `out_market_demand_attribution.csv` | Paso 2: aportación de cada consumidor al error de cada mercado |
| `out_error_origins_EUR.csv` | Paso 3: origen de cada tecnología europea, por tipo, con su bucle |
| `out_error_origins_by_sector_EUR.csv` | Paso 3 resumido por sector |

**Validación.**

| Comprobación | Resultado |
| --- | --- |
| Cociente logarítmico del grafo frente a `all_errors_output_by_tech.csv` | Diferencia máxima < 10⁻¹² en las 15.769 filas |
| Mercados intermedios que cuadran oferta y demanda antes de añadir sumideros | 90 % con diferencia < 1 % |
| Paso 1: e\_total − e\_market − e\_local | Residuo máximo 1,8 × 10⁻¹⁵ |
| Paso 3: e − (origin + B e) | Máximo 2,5 × 10⁻⁶; percentil 99, 10⁻¹³ |
| Paso 3: solución de (I − B) e = origin frente a e observado | Diferencia máxima 2,5 × 10⁻⁶ |

Estas cifras salen de la réplica en Python del script, ejecutada sobre tus datos. La versión en R imprime el residuo del sistema al ejecutarse.

**Primeros resultados.** En el sector `electricity` europeo, unas cuatro quintas partes del error vienen heredadas de la demanda (born\_here\_ratio ≈ 0,21). En cemento, química, otra industria y calefacción residencial con carbón, el error nace en la demanda final (born\_here\_ratio = 1). En las tecnologías de generación con gas o carbón que aparecen en la query, el error es casi todo local.

## 9. Limitaciones e interpretación

La descomposición es contable: dice por dónde pasa el error según la estructura de GCAM, no qué parámetro lo causa. Pasar del "dónde" al "por qué" corresponde a la fase de sensibilidad.

- **Identidad, no causalidad.** Que el 80 % del error de un nodo sea heredado significa que se explica aritméticamente por la demanda de su mercado. Ese error de demanda puede venir a su vez de un parámetro mal calibrado aguas abajo, que es lo que el paso 3 intenta localizar.
- **Solo cantidades, no precios.** La propagación capturada es la del tirón de la demanda. En GCAM los precios también transmiten error aguas abajo: un insumo más caro cambia las cuotas de sus consumidores. Ese efecto queda dentro de e\_local y no se puede separar sin la query de precios. Con ella podría recalcularse la cuota logit con los precios observados para aislar la parte debida a parámetros.
- **Centrales eléctricas europeas.** Están dentro de un `nesting-subsector` y la query no las recoge. La generación europea solo se ve como un nodo de red regional, y su error local no se puede separar por tecnología. Hace falta añadir una query con un nivel de subsector más.
- **Margen extensivo.** Los flujos que existen en una sola ejecución no tienen error logarítmico. Su efecto se cuenta en o\_other, pero no se propaga a través de B.
- **Estructura de referencia.** El grafo se construye con la topología de la ejecución de referencia; 616 tecnologías tienen output en una sola de las dos ejecuciones.
- **La raíz de la demanda final.** o\_final indica que el error nace en la demanda final, pero no qué la mueve. Que el PIB y la población del hindcast sean los observados o los proyectados en 2015 cambia por completo la lectura: con factores observados, o\_final apunta a elasticidades y saturación; con factores proyectados, mezcla esto con el error del escenario.
- **Grafo sectorial.** En las visualizaciones a nivel región|sector, el flujo de un mercado con varios proveedores se reparte con las cuotas de referencia. Por eso el error de esas aristas refleja solo la demanda del consumidor. El script `error_propagation.R` trabaja a nivel de tecnología y no tiene esta limitación.

## 10. Referencias

Citadas de memoria; conviene verificar los datos bibliográficos antes de usarlas en un texto.

- Ang, B. W. (2004). Decomposition analysis for policymaking in energy: which is the preferred method? *Energy Policy*, 32(9). Justifica LMDI frente a otros índices (paso 2).
- Ang, B. W. (2005). The LMDI approach to decomposition analysis: a practical guide. *Energy Policy*, 33(7). Fórmulas de la media logarítmica y tratamiento de ceros.
- Leontief, W. (1936). Quantitative input and output relations in the economic system of the United States. *Review of Economics and Statistics*, 18(3). Origen de la inversa (I − A)⁻¹ (paso 3).
- Miller, R. E. y Blair, P. D. (2009). *Input-Output Analysis: Foundations and Extensions*, 2.ª ed. Cambridge University Press. Desarrollo en serie de la inversa y multiplicadores.
- Tarjan, R. (1972). Depth-first search and linear graph algorithms. *SIAM Journal on Computing*, 1(2). Algoritmo de componentes fuertemente conexas (paso 4).
- Documentación de GCAM (JGCRI, gcam-doc): representación de mercados, función logit anidada y equilibrio, que justifican la identidad del paso 1.

Código: `error_propagation.R` y los scripts de `_build/` en `Hindcasting\5. Error propagation graph`.
