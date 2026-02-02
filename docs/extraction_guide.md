# GUIA: Script Completo de Extração de Dados

## 📄 Arquivo: complete_extraction.R

Este script executa o **pipeline completo** de extração de dados do OpenAlex:

```
OpenAlex API → Bronze (JSONL) → Silver (Parquet) → Gold (Agregações)
```

---

## 🚀 USO RÁPIDO

### Opção 1: Pipeline Completo (Recomendado)

```r
# Carregar o script
source("R/complete_extraction.R")

# Executar tudo
results <- run_complete_extraction(
  max_pages = NULL,  # Sem limite (busca tudo)
  polite_email = "seu.email@example.com"  # Opcional mas recomendado
)
```

**Tempo estimado:** 15-20 minutos  
**Resultado:** 292 documentos processados

---

### Opção 2: Teste Rápido (Pequena Amostra)

```r
source("R/complete_extraction.R")

# Buscar apenas 2 páginas (~400 documentos)
results <- run_complete_extraction(max_pages = 2)
```

**Tempo estimado:** 2-3 minutos  
**Útil para:** Testar o pipeline antes da extração completa

---

### Opção 3: Por Etapas

```r
source("R/complete_extraction.R")

# ETAPA 1: Bronze (API → JSONL)
bronze_path <- fetch_openalex_bronze(
  filter_string = build_query_BR(),
  max_pages = 5
)

# ETAPA 2: Silver (JSONL → Parquet)
silver_paths <- build_silver_tables(bronze_path)

# ETAPA 3: Gold (Agregações)
gold_paths <- build_gold_tables()
```

---

## 📊 O QUE O SCRIPT FAZ

### BRONZE LAYER (Extração)

**Entrada:** Nenhuma  
**Saída:** `data/bronze/openalex_bronze_YYYY-MM-DD_HHMMSS.jsonl`

**O que faz:**
1. Conecta à API OpenAlex
2. Aplica filtros:
   - País: Brasil
   - Idioma: Português
   - Período: 2014-2023
   - Termos: "direito à alimentação", "segurança alimentar", etc.
3. Faz download dos dados brutos
4. Salva em formato JSONL (1 objeto JSON por linha)

**Rate limiting:** 0.2 segundos entre requisições (respeita limites da API)

---

### SILVER LAYER (Normalização)

**Entrada:** `data/bronze/*.jsonl`  
**Saída:** 4 tabelas Parquet + 4 CSV

| Tabela | Descrição | Campos principais |
|--------|-----------|-------------------|
| `works.parquet` | Metadados dos documentos | work_id, doi, title, year, citations |
| `authorships.parquet` | Autoria e afiliações | work_id, author_id, author_name |
| `concepts.parquet` | Conceitos temáticos | work_id, concept_id, concept_name, score |
| `references.parquet` | Referências bibliográficas | work_id, referenced_work_id |

**O que faz:**
1. Lê o arquivo JSONL
2. Parse de cada objeto JSON
3. Extrai campos relevantes
4. Normaliza em tabelas relacionais
5. Trata valores NULL
6. Salva em Parquet (eficiente) + CSV (compatível)

---

### GOLD LAYER (Agregações)

**Entrada:** `data/silver/*.parquet`  
**Saída:** 5 tabelas analíticas

| Tabela | Descrição |
|--------|-----------|
| `production_by_year.parquet` | Documentos por ano (2014-2023) |
| `production_by_type.parquet` | Documentos por tipo (article, thesis, etc.) |
| `top_sources.parquet` | Top 20 revistas/fontes |
| `top_concepts.parquet` | Top 200 conceitos mais frequentes |
| `concept_cooccurrence_edges.parquet` | Rede de co-ocorrência (peso ≥5) |

**O que faz:**
1. Carrega tabelas Silver
2. Calcula agregações
3. Constrói rede de conceitos
4. Filtra edges (mínimo 5 co-ocorrências)
5. Salva resultados prontos para análise

---

## 🔧 PARÂMETROS DISPONÍVEIS

### `run_complete_extraction()`

```r
run_complete_extraction(
  max_pages = NULL,          # Limite de páginas (NULL = sem limite)
  polite_email = "",         # Seu email (para rate limit preferencial)
  bronze_dir = "data/bronze",
  silver_dir = "data/silver",
  gold_dir = "data/gold"
)
```

### `fetch_openalex_bronze()`

```r
fetch_openalex_bronze(
  filter_string = NULL,      # Query OpenAlex (NULL = usa padrão BR)
  out_dir = "data/bronze",
  per_page = 200,            # Resultados por página (max 200)
  max_pages = NULL,          # Limite de páginas
  select_fields = c(...),    # Campos a buscar
  polite_email = ""          # Seu email
)
```

### `build_query_BR()`

**Query padrão** para o Brasil:
- Termos em PT: "direito à alimentação", "segurança alimentar", etc.
- Termos em EN: "right to food", "food security", etc.
- Filtros: `country_code:BR`, `language:pt`, anos 2014-2023

**Personalize** editando a função se necessário.

---

## 📁 ESTRUTURA DE SAÍDA

```
data/
├── bronze/
│   └── openalex_bronze_2025-02-02_143022.jsonl  (~10-20 MB)
├── silver/
│   ├── works.parquet          (292 linhas)
│   ├── works.csv
│   ├── authorships.parquet    (~478 linhas)
│   ├── authorships.csv
│   ├── concepts.parquet       (~1847 linhas)
│   ├── concepts.csv
│   ├── references.parquet     (~4521 linhas)
│   └── references.csv
└── gold/
    ├── production_by_year.parquet
    ├── production_by_type.parquet
    ├── top_sources.parquet
    ├── top_concepts.parquet
    └── concept_cooccurrence_edges.parquet
```

---

## ⚡ DICAS DE DESEMPENHO

### 1. Use max_pages para testes
```r
# Teste rápido: 2 páginas (~400 works)
results <- run_complete_extraction(max_pages = 2)
```

### 2. Forneça seu email
```r
# OpenAlex pode dar rate limit preferencial
results <- run_complete_extraction(
  polite_email = "seu@email.com"
)
```

### 3. Reprocesse sem baixar de novo
```r
# Se já tem o bronze, pule a etapa de download
silver_paths <- build_silver_tables("data/bronze/arquivo_existente.jsonl")
gold_paths <- build_gold_tables()
```

---

## 🐛 SOLUÇÃO DE PROBLEMAS

### Erro: "HTTP 403" ou "Rate limit"
**Solução:** Adicione seu email
```r
results <- run_complete_extraction(polite_email = "seu@email.com")
```

### Erro: "Cannot open file"
**Solução:** Certifique-se que está no diretório correto
```r
getwd()  # Verificar
setwd("/caminho/para/projeto")  # Ajustar se necessário
```

### Erro: "Package not found"
**Solução:** Instale pacotes
```r
install.packages(c("httr", "jsonlite", "dplyr", "arrow", "purrr", "stringr"))
```

### Bronze muito grande (>100 MB)
**Normal!** O arquivo JSONL pode ser grande. Ele será compactado no Parquet.

### Quer apenas alguns documentos?
```r
# Limite drástico para teste
results <- run_complete_extraction(max_pages = 1)  # ~200 works
```

---

## 📊 VERIFICAR RESULTADOS

### Verificação Rápida

```r
library(arrow)
library(dplyr)

# Carregar works
works <- read_parquet("data/silver/works.parquet")

# Estatísticas básicas
nrow(works)  # Total de documentos
table(works$publication_year)  # Por ano
table(works$type)  # Por tipo
summary(works$cited_by_count)  # Citações

# Ver primeiras linhas
head(works)
```

### Análise Completa

Use o script de validação:
```r
source("R/validation/R_validation_metrics.R")
results <- run_all_validations()
```

---

## 🔄 ATUALIZAÇÕES FUTURAS

Para atualizar os dados (ex: adicionar 2024):

```r
# Edite build_query_BR() para mudar datas
build_query_BR <- function() {
  # ... 
  ",to_publication_date:2024-12-31"  # ← Mude aqui
}

# Execute novamente
results <- run_complete_extraction()
```

---

## 📝 EXEMPLO COMPLETO

```r
# ============================================
# EXEMPLO: Extração completa com todas opções
# ============================================

# 1. Carregar script
source("R/complete_extraction.R")

# 2. Configurar
polite_email <- "sostenes@exemplo.com"
max_pages <- NULL  # Sem limite (busca tudo)

# 3. Executar
cat("Iniciando extração...\n")
results <- run_complete_extraction(
  max_pages = max_pages,
  polite_email = polite_email
)

# 4. Verificar
cat("\nVerificando resultados...\n")
library(arrow)
works <- read_parquet(results$silver$works)
cat("Total de documentos:", nrow(works), "\n")
cat("Anos:", range(works$publication_year, na.rm = TRUE), "\n")

# 5. Estatísticas
cat("\nDocumentos por ano:\n")
print(table(works$publication_year))

cat("\n✓ Extração concluída com sucesso!\n")
```

---

## ⏱️ ESTIMATIVAS DE TEMPO

| Atividade | max_pages | Tempo | Documentos |
|-----------|-----------|-------|------------|
| Teste pequeno | 1 | ~1 min | ~200 |
| Teste médio | 5 | ~3 min | ~1000 |
| Amostra | 10 | ~5 min | ~2000 |
| **Completo (BR 2014-23)** | **NULL** | **15-20 min** | **~292** |

*Tempos variam conforme conexão e carga da API*

---

## 📚 REFERÊNCIAS

- OpenAlex API Docs: https://docs.openalex.org/
- Arrow/Parquet: https://arrow.apache.org/docs/r/
- Rate limits: https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication

---

**Script criado:** 02/02/2025  
**Versão:** 1.0.0  
**Testado com:** R 4.3.2, OpenAlex API 2025-01-01
