# Fracture numérique et accès aux soins

L'objectif de [cette application](https://drees.shinyapps.io/Fracture_Numerique_Sante/) est de faciliter l'identification de **zones mal couvertes** en réseau mobile (3G/4G) et en offre de soin.

# Les données

Les deux champs numérique et santé s'appuient sur des sources diverses 

## Données d'accessibilité au numérique

Le principal producteur de données est l'**ARCEP**. 

On s'appuie sur leur carte de **couverture du territoire** en **3G** et **4G** pour calculer pour chaque commune un pourcentage de la surface de la commune couverte pas la 4G ou 3G de chaque opérateur. 

On prend ensuite la meilleure couverture parmi les 4 opérateur.


## Données d'accessibilités aux professionnels de santé

L'**APL** : accessibilité potentielle localisée est calculée par la **DREES** et permet de tenir compte de trois paramètres dans le calcul de l'accès aux soins :

- **offre** des professionnels de santé (en ETP) à proximité
- **demande** potentielle (nombre de patients de la commune)
- **distance** des patients potentiels aux professionnels de santé qui permet de pondérée l'offre vis à vis de la demande.

L'APL est déclinée pour 4 professionnels de santé au moins : 

- Médecins généralistes
- Masseurs Kinésithérapeutes
- Infirmiers
- Sages-femmes


# Fonctionnement de l'application

Déployée sur **shinyapps.io**, cette application s'appuie sur des données qui ne sont pas stockées sur de dépôt afin de limiter sa taille et aussi d'accélérer le déploiement et le chargement de l'application.

Les données sont stockées sur un compte Google Drive de la DREES, si vous souhaitez faire tourner l'application localement, merci de solliciter les *maintainers* afin de récupérer le script `ident_get_ggdrive_data.R` ainsi que le dossier `.secrets/`
