## Avaliação técnica Quaest Pesquisa e Consultoria (Data Scientist)

Os primeiros passos realizados nesta análise foram alguns ajustes feitos no banco de dados. Segue abaixo estes passos.
```R
#pacotes utilizados
library(ggplot2)
library(questionr)
library(treemapify)
library(treemap)

#diretório
setwd("~/avaliacao_cientista_de_dados_quaest")

#função
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#dados
dados = read.csv("bd_surveyquaest.csv", sep = ";")

#criação de variáveis
dados$idade2 = cut(dados$idade,breaks=c(17, 35, 45, 65, 87), right = FALSE)
dados$renda2 = ifelse(dados$rendaf == "Até R$ 1.045,00 (até 1 SM)", "Até 1SM",
                      ifelse(dados$rendaf == "De R$ 1.046,00 a R$ 2.090,00 (+ de 1SM até 2 SM)", "+ 1 até 2SM",
                             ifelse(dados$rendaf == "De R$ 2.091,00 a R$ 3.135,00 (+ de 2SM até 3 SM)", "+ 2 até 3SM",
                                    ifelse(dados$rendaf == "De R$ 3.136,00 a R$ 5.225,00 (+ de 3SM até 5 SM)", "+ 3 até 5SM",
                                           ifelse(dados$rendaf == "De R$ 5.226,00 a R$ 10.450,00 (+ de 5SM até 10 SM)", "+ 5 até 10SM",
                                                  ifelse(dados$rendaf == "De R$ 10.451,00 a R$ 15.675,00 (+ de 10SM até 15 SM)", "+ 10 até 15SM" , "+ 15"))))))

dados$esc2 = dados$esc
dados$esc2 = gsub("Ensino ", "", dados$esc2 )
dados$esc2 = gsub("completo", "comp.", dados$esc2 )
dados$esc2 = gsub("incompleto", "incomp.", dados$esc2 )
dados$esc2 = gsub("fundamental", "Fundam.", dados$esc2 )
dados$esc2 = gsub("superior", "Sup.", dados$esc2 )
dados$esc2 = gsub("Sem instrução e menos de 1 ano de estudo", "Sem instr. e menos de 1 ano de estudo", dados$esc2 )
dados$esc2 = firstup(dados$esc2)
dados$count = 1

#dados para a figura 2
dados2 = dados[-which(dados$aval_gov == "NS/NR"),]
dados2$aval_gov2 = ifelse((dados2$aval_gov == "Boa") | (dados2$aval_gov == "Ótima ") | 
                            (dados2$aval_gov == "Regular positiva"), "Positiva",  "Negativa") 
```



























You can use the [editor on GitHub](https://github.com/guilhermeaguilar/quaest_test/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.
testes 
Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/guilhermeaguilar/quaest_test/settings/pages). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
