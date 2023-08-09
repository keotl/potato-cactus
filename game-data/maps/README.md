# Map data
Contains both terrain data (for building collision maps) and game
object placements. potato-cactus reads all `*.objects.json` and
`*.terrain.json` files in this folder. Attribute naming follows
refactor317.

## *.objects.json
```json
[{
    "objectId": 879,
    "x": 3221,
    "y": 3210,
    "plane": 0,
    "type": 10,
    "orientation": 0
}]
```

## *.terrain.json
```json
[{ "x": 3204, "y": 3207, "plane": 0, "renderRuleFlags": 4 }]
```
