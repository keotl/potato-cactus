"""
Output on 317 game data
=======================

Found 1166825 object placements.
All items were between (x=1856,y=2690) and (x=3832,y=10367), spanning a (dx=1976) by (dy=7677) area, for a total of 15169752 tiles.
Of these, 49773 were wall placements, and 1197209 tiles were occupied by objects.
This means that 7.89 % of tiles are occupied within the minimum spanning area.
Therefore, storing each occupied tile and wall using one Word64 in a hashset would take 9.51 MiB of memory.
Alternatively, storing each occupied tile in a bitmap using at best only a single bit per tile would take 1.81 MiB of memory.
Using instead 1 bit per tile and one Word64 per wall would take 2.19 MiB of memory.
Without assuming the minimum spanning area for the bitmap, the combined usage would be 5.12 MiB of memory.
By only creating a bitmap for map regions with at least one occupied tile or wall, the usage would be 0.64 MiB of memory.

"""
import glob
import json

count = 0
max_x = 0
min_x = 999999
max_y = 0
min_y = 999999
occupied_tiles = 0
walls = 0
defs = {}
regions_with_items = set()

with open("game-data/definitions/objects.json") as f:
    read_defs = json.load(f)
    for obj in read_defs:
        defs[obj["id"]] = obj

for file in glob.glob("game-data/maps/*.objects.json"):
    with open(file, "r") as f:
        content = json.load(f)
        count += len(content)
        for item in content:
            if item["x"] > max_x:
                max_x = item["x"]
            if item["x"] < min_x:
                min_x = item["x"]
            if item["y"] > max_y:
                max_y = item["y"]
            if item["y"] < min_y:
                min_y = item["y"]

            definition = defs[item["objectId"]]
            if definition["solid"]:
                region_x = item["x"] // 64
                region_y = item["y"] // 64
                regions_with_items.add((region_x, region_y))

            if definition["wall"]:
                walls += 1
            elif definition["solid"]:
                occupied_tiles += definition["sizeX"] * definition["sizeY"]

print(f"Found {count} object placements.")
print(
    f"All items were between (x={min_x},y={min_y}) and (x={max_x},y={max_y}), "
    f"spanning a (dx={max_x - min_x}) by (dy={max_y - min_y}) area, for a total "
    f"of {(max_x - min_x)*(max_y - min_y)} tiles.")

print(
    f"Of these, {walls} were wall placements, and {occupied_tiles} tiles were occupied by objects."
)
print(
    f"This means that {occupied_tiles / ((max_x - min_x)*(max_y - min_y)) * 100:.2f} % of tiles are occupied within the minimum spanning area."
)
print(
    f"Therefore, storing each occupied tile and wall using one Word64 in a hashset would take {((walls + occupied_tiles) * 8) / 2**20:.2f} MiB of memory."
)
print(
    f"Alternatively, storing each occupied tile in a bitmap using at best only a single bit per tile would take {((max_x - min_x)*(max_y - min_y)) / 8 / 2**20:.2f} MiB of memory."
)
print(
    f"Using instead 1 bit per tile and one Word64 per wall would take {((max_x - min_x)*(max_y - min_y)) / 8 / 2**20 + (walls) * 8 / 2**20:.2f} MiB of memory."
)
print(
    f"Without assuming the minimum spanning area for the bitmap, the combined usage would be {((max_x)*(max_y)) / 8 / 2**20 + (walls) * 8 / 2**20:.2f} MiB of memory."
)
print(
    f"By only creating a bitmap for map regions with at least one occupied tile or wall, the usage would be "
    f"{(len(regions_with_items)* 64 * 64 / 8  + walls * 8)/ 2**20:.2f} MiB of memory."
)
