# INSTRUÇÕES: Como usar o script de validação

## 1. CONFIGURAÇÃO INICIAL

Antes de rodar o script, certifique-se de que você tem:

```r
# Pacotes necessários
install.packages(c("dplyr", "arrow", "igraph", "ggplot2", "stringr"))
```

## 2. EXECUTAR TODAS AS VALIDAÇÕES DE UMA VEZ

```r
# Carregar o script
source("R_validation_metrics.R")

# Rodar todas as validações
results <- run_all_validations(
  silver_dir = "data/silver",
  gold_dir = "data/gold",
  out_dir = "outputs/validation"
)
```

Isso vai gerar:

### Arquivos CSV:
- `outputs/validation/table2_completeness.csv` → Para Table 2 no paper
- `outputs/validation/table_schema_works.csv` → Para Table 3 no paper
- `outputs/validation/table_schema_authorships.csv` → Para Table 4 no paper
- `outputs/validation/table_schema_concepts.csv` → Para Table 5 no paper
- `outputs/validation/table_schema_references.csv` → Para Table 6 no paper

### Arquivos de texto:
- `outputs/validation/consistency_results.txt` → Para seção Technical Validation

### Figuras:
- `outputs/figures/fig3_degree_distribution.png` → Figure 3 no paper
- `outputs/figures/fig4_completeness_heatmap.png` → Figure 4 no paper (opcional)

### Objeto R:
- `outputs/validation/network_metrics.rds` → Métricas para seção Network Validation

## 3. EXECUTAR VALIDAÇÕES INDIVIDUAIS

Se você quiser rodar cada análise separadamente:

```r
source("R_validation_metrics.R")

# 1. Completeness
completeness <- calculate_completeness("data/silver")
print(completeness)

# 2. Consistency
consistency <- check_consistency("data/silver")
print(consistency)

# 3. Schema documentation
schemas <- document_schema("data/silver")
print(schemas$works)

# 4. Network validation
network_metrics <- validate_network("data/gold")
print(network_metrics)

# 5. Generate degree distribution plot
plot_degree_distribution("data/gold", "outputs/figures")

# 6. Generate completeness heatmap
plot_completeness_heatmap("data/silver", "outputs/figures")
```

## 4. USAR OS RESULTADOS NO PAPER

### Para Table 2 (Completeness Statistics):

```r
completeness <- read.csv("outputs/validation/table2_completeness.csv")
print(completeness)
```

Copie e cole essa tabela na seção **Technical Validation > Completeness**.

### Para Tables 3-6 (Schema Documentation):

```r
works_schema <- read.csv("outputs/validation/table_schema_works.csv")
print(works_schema)
```

Copie e cole essas tabelas na seção **Data Records > Schema Documentation**.

### Para Consistency Results:

```r
consistency <- readLines("outputs/validation/consistency_results.txt")
cat(paste(consistency, collapse = "\n"))
```

Copie e cole esse texto na seção **Technical Validation > Consistency**.

### Para Network Metrics:

```r
network_metrics <- readRDS("outputs/validation/network_metrics.rds")
print(network_metrics)
```

Use essas métricas para escrever a seção **Technical Validation > Network Validation**.

Exemplo de parágrafo:

"Concept co-occurrence network properties:
- Nodes: {network_metrics$n_nodes} concepts
- Edges: {network_metrics$n_edges} co-occurrences (weight ≥ 5)
- Average degree: {round(network_metrics$avg_degree, 1)}
- Network density: {round(network_metrics$network_density, 3)}
- Modularity (Louvain): {round(network_metrics$modularity, 2)} (indicates strong community structure)

Clustering stability: {round(network_metrics$stability_pct, 1)}% of nodes received identical 
cluster assignments across 10 runs with different random seeds."

## 5. SOLUÇÃO DE PROBLEMAS

### Erro: "concept_cooccurrence_edges.parquet not found"

Isso significa que você precisa rodar o pipeline completo primeiro:

```r
source("R/scripts/00_setup.R")
targets::tar_make()
```

### Erro: "cannot open file 'data/silver/works.parquet'"

Certifique-se de que você está no diretório raiz do projeto:

```r
getwd()  # Deve mostrar o caminho do projeto
setwd("/caminho/para/seu/projeto")  # Se necessário
```

### Figuras não aparecem

Verifique se o diretório de saída foi criado:

```r
dir.create("outputs/figures", recursive = TRUE)
```

## 6. PRÓXIMOS PASSOS

Depois de rodar as validações:

1. ✅ Abra os arquivos CSV gerados
2. ✅ Copie as tabelas para o manuscrito do data paper
3. ✅ Copie as figuras PNG para o manuscrito
4. ✅ Use as métricas para escrever os parágrafos de validação
5. ✅ Revise todos os números para garantir que estão corretos

## 7. EXEMPLO COMPLETO DE USO

```r
# Passo 1: Configurar ambiente
library(dplyr)
library(arrow)
library(igraph)
library(ggplot2)

# Passo 2: Carregar script
source("R_validation_metrics.R")

# Passo 3: Rodar todas as validações
results <- run_all_validations()

# Passo 4: Verificar outputs
list.files("outputs/validation")
list.files("outputs/figures")

# Passo 5: Visualizar resultados
print(results$completeness)
print(results$network_metrics)

# Passo 6: Exportar para LaTeX (opcional)
library(knitr)
kable(results$completeness, format = "latex")
```

## 8. ESTIMATIVA DE TEMPO

- Configuração inicial: 5 minutos
- Execução completa: 2-5 minutos
- Copiar resultados para o paper: 15-20 minutos
- **Total: ~30 minutos**
