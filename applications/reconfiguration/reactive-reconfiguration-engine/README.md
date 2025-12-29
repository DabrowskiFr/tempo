# reactive-reconfiguration-engine

Noyau de recherche de reconfiguration a la Concerto-D, base sur Tempo.

L'idee centrale: decrire un systeme et ses transactions de reconfiguration
dans des fichiers texte (style Concerto-D simplifie), puis laisser le moteur:
- ordonnancer les transactions,
- detecter les conflits,
- verifier les preconditions,
- committer ou rollback via compensation,
- calculer des invariants et produire des traces.

Le moteur inclut un validateur statique qui signale avant execution:
- composants/groupes inconnus
- groupes vides
- references invalides dans `depends`/`bind`
- cycles de dependances
- transactions dupliquees ou vides
- contraintes `within` invalides
- operations non resolvables (templates ignorees)

## Demonstration reactive Tempo (objectif principal)

La boucle interactive utilise maintenant directement les primitives reactives de Tempo:
- `Tempo.execute` (runtime synchrone par instants)
- `parallel` (processus concurrents)
- `watch` + signal `stop` (annulation synchrone)
- `Game.every_n` (horloge logique pour le tick moteur)
- `Event_bus.channel` + `publish` + `await_batch` (bus d'evenements agreges intra-instant)
- `Scene` (gestion synchrone des modes runtime `normal/degraded/recovery`)
- `await` / `emit` (synchronisation explicite entre processus)

Architecture reactive:
- un processus d'entree convertit les touches en evenements metier
- un processus horloge emet `Tick` a chaque instant logique
- un processus reducteur applique les batches d'evenements et emet la vue

## Fichiers declaratifs

Le moteur charge par defaut:
- `applications/reconfiguration/reactive-reconfiguration-engine/spec/system.concerto`
- `applications/reconfiguration/reactive-reconfiguration-engine/spec/reconfig.concerto`

### `system.concerto`

Syntaxe:
- `component <Nom> <running|stopped|updating>`
- `group <NomGroupe> <Comp1> <Comp2> ...`
- `depends <Child> <Parent>`
- `bind <Child> <Parent>`
- `policy <conservative|balanced|aggressive>` (preset)
- `policy_profile <name>` (nom declaratif)
- `max_parallel <int>`
- `tx_deadline <int>`
- `auto_period <int>`
- `rollback_on_failure <true|false>`
- `scenario <baseline|lossy|partitioned>`

Exemple:

```txt
component Proxy running
component API running
component DB running
component Worker stopped

group frontend Proxy API
group backend DB Worker

depends API DB
depends Worker API
depends Proxy API

bind API DB
bind Proxy API

policy balanced
policy_profile lab_profile
max_parallel 2
tx_deadline 300
auto_period 220
rollback_on_failure true
scenario baseline
```

### `reconfig.concerto`

Syntaxe:
- `transaction <name> [deadline <int>]`
- operations:
  - `start <Component>`
  - `stop <Component>`
  - `update <Component>`
  - `bind <A> <B>`
  - `unbind <A> <B>`
  - `add <Component>`
  - `remove <Component>`
- extension:
  - cibler un groupe avec `@nom` (ex: `start @backend`)
  - contrainte temporelle par operation: `within <int>`
- `end`

Exemple:

```txt
transaction start-backend deadline 260
start @backend within 120
bind Worker API within 160
end
```

## Utilisation interactive

```sh
dune exec ./applications/reconfiguration/run -- reactive-reconfiguration-engine
```

Touches:
- `T`: enqueue la transaction suivante du `reconfig.concerto`
- `R`: recharge `system.concerto` + `reconfig.concerto` a chaud
- `P`: toggle partition
- `L` / `K`: loss +/-
- `Y`: cycle policy
  - en mode interactif, `Y` fait tourner les presets; le profil declaratif vient du fichier system
- `U`: cycle scenario

## Utilisation headless (experiences)

```sh
dune exec ./applications/reconfiguration/run -- reactive-reconfiguration-engine -- --headless --steps 2400 --seed 7 --policy balanced --scenario baseline
```

Options:
- `--steps <n>`
- `--seed <n>`
- `--policy conservative|balanced|aggressive`
- `--scenario baseline|lossy|partitioned`
- `--system-file <path>`
- `--reconfig-file <path>`
- `--trace-dir <path>`

Sorties:
- traces evenementielles CSV
- resume JSON
dans `applications/reconfiguration/reactive-reconfiguration-engine/traces/`

Le nombre de diagnostics de validation est exporte dans le JSON (`spec_issues`)
et affiche dans le HUD (`spec_issues=<n>`).
