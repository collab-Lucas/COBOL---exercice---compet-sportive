# COBOL---exercice---compet-sportive

# 🏃‍♂️ Projet COBOL - Gestion de Compétitions Sportives

## 🎓 Contexte pédagogique

Ce projet a été réalisé **en cours** dans le cadre d'un exercice d’apprentissage du langage **COBOL**.  
L’objectif était de se familiariser avec la gestion de fichiers, la structure modulaire d’un programme COBOL, et la manipulation de données via des fichiers `.dat`.

## 📁 Structure du projet

- `athletes.cob` : gestion des athlètes
- `competitions.cob` : gestion des compétitions
- `courses.cob` : gestion des courses
- `participants.cob` : gestion des participants
- `*.dat` : fichiers de données binaires utilisés par les programmes

## 🧪 Compilation & Exécution

> ✅ Prérequis : [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

### Compilation

```bash
cobc -x -o athletes athletes.cob
cobc -x -o competitions competitions.cob
cobc -x -o courses courses.cob
cobc -x -o participants participants.cob

### Exécution
./athletes
