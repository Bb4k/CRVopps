# CRVopps - Continuous Random Variables opperations
[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg?style=flat-square)](https://github.com/Bb4k/CRVopps/blob/main/README.md#contributors)
[![license](https://img.shields.io/github/license/DAVFoundation/captain-n3m0.svg?style=flat-square)](https://github.com/Bb4k/CRVopps/blob/main/LICENSE)


This project was developed during the university and served as the final project for the subject Probability and Statistics. The aim of the project is to develop an R package for working with continuous random variables.

## R packages used in this project (they must be installed in order to use the package)

   - devtools
   - roxygen2
   - pcubature
   - statip
   - pracma
   - Ryacas

## Package commands

   - for build use CTRL+SHIFT+B
 
## Project requirements (in Romanian)

- [X] 1) Fiind dată o funcție f , introdusă de utilizator, determinarea unei constante de
normalizare k. Ȋn cazul ȋn care o asemenea constantă nu există, afișarea unui mesaj
corespunzător către utilizator.
- [X] 2) Verificarea dacă o funcție introdusă de utilizator este densitate de probabilitate.
- [X] 3) Crearea unui obiect de tip variabilă aleatoare continuă pornind de la o densitate de
probabilitate introdusă de utilizator. Funcția trebuie să aibă opțiunea pentru variabile
aleatoare unidimensionale și respectiv bidimensionale.
- [X] 4) Reprezentarea grafică a densității și a funcției de repartiție pentru diferite valori ale
parametrilor repartiției. Ȋn cazul ȋn care funcția de repartiție nu este dată ȋntr-o formă
explicită(ex. repartiția normală) se acceptă reprezentarea grafică a unei aproximări a
acesteia.
- [X] 5) Calculul mediei, dispersiei și a momentelor inițiale și centrate pȃnă la ordinul 4(dacă
există). Atunci cȃnd unul dintre momente nu există, se va afișa un mesaj
corespunzător către utilizator.
- [X] 6) Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție
continuă cunoscută iar g este o funcție continuă precizată de utilizator.
- [x] 7) Crearea unei funcții P care permite calculul diferitelor tipuri de probabilități asociate
unei variabile aleatoare continue(similar funcției P din pachetul discreteRV)
- [X] 8) Afișarea unei “fișe de sinteză” care să conțină informații de bază despre respectiva
repartiție(cu precizarea sursei informației!). Relevant aici ar fi să precizați pentru ce e
folosită ȋn mod uzual acea repartiție, semnificația parametrilor, media, dispersia etc.
- [ ] 9) Generarea a n valori(unde n este precizat de utilizator!) dintr-o repartiție de variabile
aleatoare continue( solicitați material suport pentru partea de simulare).
- [X] 10) Calculul covarianței și coeficientului de corelație pentru două variabile aleatoare
continue(Atenție:Trebuie să folosiți densitatea comună a celor două variabile
aleatoare!)
- [X] 11) Pornind de la densitatea comună a două variabile aleatoare continue, construirea
densităților marginale și a densităților condiționate.
- [X] 12) Construirea sumei și diferenței a două variabile aleatoare continue
independente(folosiți formula de convoluție)

## Contributors

<table>
  <tr>
    <td align="center"><a href="https://github.com/DragosBalmau"><img src="https://avatars.githubusercontent.com/u/30263894?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Dragoș C. Bălmău</b></sub></a></td>
    <td align="center"><a href="https://github.com/TIPYexe"><img src="https://avatars.githubusercontent.com/u/53595545?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Robert A. Nicolescu</b></sub></a></td>
    <td align="center"><a href="https://github.com/VictorAndreiCotescu"><img src="https://avatars.githubusercontent.com/u/63092892?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Victir A. Cotescu</b></sub></a></td>
  </tr>
</table>
