from PIL import Image
import numpy as np
import os
from sys import argv

if len(argv) < 2:
    print("Usage: convert_image.py file.png ...files")
    exit()
colors = [
    [1000, 1000, 1000],
    [0, 0, 0],
    [34, 32, 52],
    [69, 40, 60],
    [102, 57, 49],
    [143, 86, 59],
    [223, 113, 38],
    [217, 160, 102],
    [238, 195, 154],
    [251, 242, 54],
    [153, 229, 80],
    [106, 190, 48],
    [55, 148, 110],
    [75, 105, 47],
    [82, 75, 36],
    [50, 60, 57],
    [63, 63, 116],
    [48, 96, 130],
    [91, 110, 225],
    [99, 155, 255],
    [95, 205, 228],
    [203, 219, 252],
    [255, 255, 255],
    [155, 173, 183],
    [132, 126, 135],
    [105, 106, 106],
    [89, 86, 82],
    [118, 66, 138],
    [172, 50, 50],
    [217, 87, 99],
    [215, 123, 186],
    [143, 151, 74],
    [138, 111, 48],
]

for arg in argv[1:]:
    im = Image.open(arg)
    im = im.convert("RGBA")
    matrix = np.array(im)

    print("[|[|", end="")

    for y in range(len(matrix)):
        row = matrix[y]
        for pixel in row:
            if pixel[3] == 0:
                print("0;", end="")
                continue
            for i in range(len(colors)):
                if (
                    colors[i][0] == pixel[0]
                    and colors[i][1] == pixel[1]
                    and colors[i][2] == pixel[2]
                ):
                    print(f"{i};", end="")
                    break
        if y < matrix.shape[0] - 1:
            print("|]; [|", end="")

    print("|]|];")
