# Máster en Inteligencia Artificial

Repositorio con proyectos realizados durante el **Máster en Inteligencia Artificial**, demostrando experiencia en análisis de datos, machine learning, estadística y deep learning.

## Proyectos Destacados

### 1. **Predicción de Precios de Viviendas** 
**Carpeta:** `maths_statistics/ejercicio1/`

- **Tecnologías:** R, Statistical Learning
- **Técnicas aplicadas:**
  - Análisis Exploratorio de Datos (EDA)
  - Regresión Lineal con Regularización (Lasso, Ridge)
  - Reducción de Dimensionalidad (PCA)
  - Feature Engineering y selección de variables
  
- **Dataset:** 1,460 viviendas con 81 características
- **Resultados:** RMSE optimizado mediante regularización y transformación de features

**Archivos:**
- `script.R` - Análisis completo
- `script2.R` - Validación y refinamientos
- `informe_latex/` - Documentación formal en LaTeX

---

### 2. **Clasificación de Arritmias Cardíacas con ECG** 
**Carpeta:** `maths_statistics/ejercicio2/`

- **Tecnologías:** Python, Jupyter, Scikit-learn
- **Técnicas aplicadas:**
  - Análisis de señales ECG (electrocardiogramas)
  - Procesamiento de series temporales
  - Reducción de dimensionalidad (PCA)
  - Árboles de Decisión (Decision Tree)
  - Clasificación Bayesiana (Naive Bayes Gaussian)
  - Validación cruzada y matriz de confusión

- **Dataset:** MIT-BIH Arrhythmia Database
  - **Train:** 87,554 registros
  - **Test:** 21,892 registros
  - **Clases:** 5 tipos de latidos (Normal, Supraventricular, Ventricular, Fusión, Ruido)
  - **Features:** 187 valores de amplitud ECG por registro

- **Resultados:** 
  - Decision Tree: Precisión 96%
  - Naive Bayes: Sensibilidad equilibrada entre clases

**Archivos:**
- `Feedback_ECG_JuanManuel.ipynb` - Análisis completo con visualizaciones
- `Feedback_ECG_JuanManuel.tex` - Informe formal en LaTeX

---

### 3. **Análisis Exploratorio y Técnicas Avanzadas**
**Carpeta:** `maths_statistics/unidad1/` y `unidad2/`

- **Unidad 1:** Análisis de componentes principales, correlaciones, análisis descriptivo
- **Unidad 2:** Regresión múltiple, regularización, análisis de datos sintéticos

---

## 🛠️ Stack Técnico

| Área | Tecnologías |
|------|-------------|
| **Lenguajes** | R, Python |
| **ML/Stats** | Scikit-learn, Caret, GLMNet |
| **Notebooks** | Jupyter, RMarkdown |
| **Visualización** | Matplotlib, ggplot2, Seaborn |
| **Documentación** | LaTeX, Markdown |
| **Análisis de datos** | Pandas, NumPy, Tidyverse |

---

## 📁 Estructura del Repositorio

```
master_ai/
├── README.md                          # Este archivo
├── .gitignore                         # Configuración Git
├── requirements.txt                   # Dependencias Python
│
├── maths_statistics/
│   ├── ejercicio1/                    # Predicción de precios
│   │   ├── script.R
│   │   ├── train.csv
│   │   └── latex/
│   │       └── Plantilla_latex_informe.tex
│   │
│   ├── ejercicio2/                    # Clasificación ECG
│   │   ├── Feedback_ECG_JuanManuel.ipynb
│   │   ├── latex/
│   │   │   └── Feedback_ECG_JuanManuel.tex
│   │   └── [imágenes de resultados]
│   │
│   └── unidad1/, unidad2/             # Análisis exploratorio
│
├── Clasificaciones y agrupaciones/    # Clustering (K-means, DBSCAN)
├── prog_ent_ia/                       # Programación e IA
└── Regressions, deep learning/        # Regresión y deep learning
```

## Cómo usar este repositorio

### Requisitos
- Python 3.8+
- R 4.0+
- Jupyter Notebook

### Instalación
```bash
# Clonar repositorio
git clone https://github.com/[tu-usuario]/master_ai.git
cd master_ai

# Python dependencies
pip install -r requirements.txt

# Ejecutar notebooks
jupyter notebook
```

### Executar análisis R
```R
# En RStudio o R console
source("maths_statistics/ejercicio1/script.R")
```

---

## 📝 Notas

- Los archivos compilados de LaTeX (`.aux`, `.log`, etc.) están en `.gitignore`
- Los PDFs no se incluyen por tamaño; se pueden generar compilando los `.tex`
- Cada ejercicio es independiente y contiene su propia documentación

---

**Última actualización:** Marzo 2026
