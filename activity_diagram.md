```mermaid
graph TD
    A[Début] --> B[Initialiser partie]
    B --> C{Mettre 30+ points?}
    C -->|Non| D[Piocher]
    C -->|Oui| E[Poser combinaisons]
    D --> F[Joueur suivant]
    E --> G{Main vide?}
    G -->|Oui| H[🎉 Victoire!]
    G -->|Non| I{Pioche vide?}
    I -->|Oui| J[Fin - Calcul points]
    I -->|Non| F
    F --> C
```
