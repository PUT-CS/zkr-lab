import random
from PIL import Image

noise_types = [(0,1),(1,0)]
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)

def loadImage(path):
    im = Image.open(path, 'r').convert('RGB')
    width, height = im.size
    pixel_values = list(im.getdata())
    return (width, height, pixel_values)

def dividePixel(pixel):
    if (pixel == WHITE):
        u1 = random.choice(noise_types) # generate a random variation of noise
        return (u1, u1)
    else:
        random.shuffle(noise_types)
        return (noise_types[0], noise_types[1]) # if black, generate two opposite variations of noise

def divideImage(width, height, data):
    u1, u2 = [], []
    for i in range(width):
        for j in range(height):
            u1_pixel, u2_pixel = dividePixel(data[i*width + j])
            u1.append(u1_pixel)
            u2.append(u2_pixel)
    return (u1, u2)

def composeLayers(u1, u2, width, height):
    composed = []
    for i in range(width):
        for j in range(height):
            u1_mosaic = u1[i*width + j]
            u2_mosaic = u2[i*width + j]
            if u1_mosaic == u2_mosaic:
                composed.append(u1_mosaic) # they are the same (white)
            else:
                composed.append((0, 0)) # black
    return composed

def saveLayer(u, width, height, path):
    image = Image.new('1', (width*2, height)).convert('RGB') # twice the width because we separate into mosaics horizontally
    for i in range(width):
        iter = 0
        for j in range(height):
            left_pixel, right_pixel = u[i*width + j]
            image.putpixel((iter, i), WHITE if left_pixel == 1 else BLACK)
            image.putpixel((iter+1, i), WHITE if right_pixel == 1 else BLACK)
            iter += 2
    image.save(path)

def main():
    width, height, data = loadImage("./polska.png")
    assert width == height
    u1, u2 = divideImage(width, height, data)
    lastSection = composeLayers(u1, u2, width, height)
    saveLayer(u1, width, height, "u1.png")
    saveLayer(u2, width, height, "u2.png")
    saveLayer(lastSection, width, height, "endResult.png")

main()
