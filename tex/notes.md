Restructure l'introduction en intégrant les éléments des sections 2 et 3.
- Tout d'abord une présentation générale des systèmes réactifs, de la difficulté de programmation,
du problème du déterminisme, et comparaison succintes avec les callbacks.
- Ensuite une présentation générale de la programmation réactive synchrone et de
son découpage en approches control flow et data flow et en reliant aux deux grands 
représentants que sont Esterel et Lustre. C'est làa qu'on parle du lien entre déterminisme 
et instants logiques. Mettre ici les éléments bibliographiques sur les surveys.
- On rappelle que ces derniers d'un très haut niveau de sureté qui repose sur des 
restrictions de langages permettant d'organiser statiquement l'ordonnancement et de 
décider de plusieurs propriétés de corrections relatives à la causalité, le déterminisme.
- on enchaine ensuite sur la reprise de ce modèle de programmation dans le cadre de langages de 
programmation plus riches. On distingue l'approche control flow suivie par ReactiveML et l'approche 
dataflow suivie par FRP en donnant quelques éléments sur ces deux approches. 
- On se concentre sur l'approche control flow avec ReactiveML. On rappelle le lien avec Esterel, l'utilisation 
de signaux et l'hypothèse synchrone qui s'y rattache. On traite en particulier du problème de la réaction à 
l'absence instantanée ou non en rappelant qu'en Esterel celle-ci est possible du fait des restrictions du langage.
- On enchaine ensuite sur les fairthreads de Boussinot et la proposition de réaction retardée à l'absence pour régler
le problème. On situe ensuite ReactiveML dans la continuitité qui reprend le principe en l'adaptant à un langage d'ordre supérieur.
- On décrit ensuite le modèle de programmation de reactiveML et introduit Tempo comme travail qui revisite ce langage sous l'angle d'une implémentation moderne utilisant les effets algébriques d'OCAML. On présente enfin les contributions de l'article.