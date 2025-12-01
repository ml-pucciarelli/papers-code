# Papers Code Reproduction

A collection of implementations and reproductions of results from research papers. This repository documents my journey through understanding and reproducing key findings from various papers, mainly in the field of Economics.

## Overview

This project aims to:
- Implement algorithms and methods from published research papers
- Reproduce the reported results on standard datasets
- Document the implementation process and findings
- Provide a reference for understanding how these methods work in practice

## Papers Included

### Avaliação do impacto da Faixa Azul nos sinistros de trânsito em São Paulo
**Authors:** Adriano Costa<sup>1,2</sup>, Adriano Dutra<sup>1</sup>, Gustavo Theil<sup>2</sup>, Júlio Mugnol<sup>2</sup>
**Affiliation:** <sup>1</sup> Insper, <sup>2</sup> Centro de Estudos das Cidades - Laboratório Arq.Futuro do Insper, <sup>3</sup> Universidade de Chicago
**Published:** 2025
**Paper Link:** [https://repositorio.insper.edu.br/handle/11224/8028]  
**Implementation:** `insper-faixa-azul/`

**Summary:**  
The paper aims to evaluate the effectiveness of São Paulo's "Faixa Azul" (Blue Lane) intervention—a preferential lane marking for motorcyclists—in reducing traffic accidents. Using causal inference methods, specifically staggered difference-in-differences models, the researchers analyze traffic accident data to estimate the intervention's impact on road safety. The goal is to determine whether this policy measure successfully reduces motorcycle-related accidents, which disproportionately affect the city's transportation system.

**Key Contributions:**
- Estimate causal effect of "Blue Lane" intervention in reducing traffic accidents

**Reproduction Results: (Under construction)**
| Metric | Reported | Reproduced | Status |
|--------|----------|-----------|--------|
| Effect | XX.X% | XX.X% | ✅/❌ |


**Notes:**
- (Under construction)


---

## Repository Structure

```
papers-code/
├─README.md
├─LICENSE.md
├─paper1/
│  ├─README.md
│  ├─paper1.pdf
│  ├─data
│  ├─notebook
│  └─output
├─paper2/
│  └── ...
├─environment.yml
└─requirements.txt
```

## Setup & Installation

### Prerequisites
- Python 3.11+
- pip or conda

### Installation

1. Clone the repository:
```bash
git clone https://github.com/ml-pucciarelli/papers-code.git
cd papers-code
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Navigate to a specific paper's directory:
```bash
cd paper1
```

## Usage

Each paper has its own directory with implementation details. To run a specific paper's code:

Refer to each paper's README for specific instructions and parameters.

## Results Summary (Under construction)

| Paper | Main Result | Status | Dataset | Notes |
|-------|------------|--------|---------|-------|
| Paper1 | XX.X% | ✅ Reproduced | Dataset A | Slight variance due to [reason] |
| Paper 2 | XX.XX | ⚠️ Partial | Dataset B | [Notes] |

## Key Learnings

This project provided insights into:
- How to translate academic papers into working implementations
- The importance of proper hyperparameter tuning and experimental setup
- Common pitfalls and solutions when reproducing research
- Best practices for organizing reproducible machine learning projects

## Challenges Encountered (Under construction)

- **Challenge 1:** Description
- **Challenge 2:** Description
- **[Other challenges]**

## Tools & Technologies

- **Languages:** Python
- **Frameworks:** Scikit-learn
- **Other Tools:** Jupyter

## Contributing

If you'd like to contribute by:
- Adding new paper implementations
- Improving existing code
- Fixing bugs or optimizing performance
- Suggesting improvements

Feel free to open an issue or submit a pull request!

## References

- inper-faixa-azul: Costa, Adriano Borges and Theil, Gustavo and Dutra, Adriano and Mugnol, Júlio. (2025). Avaliação do impacto da Faixa Azul nos sinistros de trânsito em São Paulo (V7). Insper. https://doi.org/10.60873/FK2/ZA2ZMA

## License

This project is licensed under the MIT License - see the LICENSE.md file for details.

## Contact

For questions or discussion about any of the implementations, feel free to reach out or open an issue.

---

**Status:** 🚀 Active Development
