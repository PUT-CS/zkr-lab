from PIL import Image

def open_image(filepath):
    image = Image.open(filepath).convert('RGB')
    return image.size[0], image.size[1], list(image.getdata())

def write_image(pixels, width, height, filepath):
    image = Image.new('RGB', (width, height))
    image.putdata(pixels)
    image.save(filepath)

def text_to_binary(text):
    return ''.join(format(ord(char), '08b') for char in text)

def int_to_binary(value):
    return format(value, '08b')

def binary_to_int(binary_string):
    return int(binary_string, 2)

def extract_least_significant_bits(pixels):
    return ''.join(str(channel % 2) for pixel in pixels for channel in pixel)

def embed_message_in_pixels(message_bits, pixels):
    index = 0
    for bit_index in range(0, len(message_bits), 3):
        rgb = list(pixels[index])
        for channel_index in range(3):
            if bit_index + channel_index < len(message_bits):
                channel_binary = int_to_binary(rgb[channel_index])
                new_binary = channel_binary[:-1] + message_bits[bit_index + channel_index]
                rgb[channel_index] = binary_to_int(new_binary)
        pixels[index] = tuple(rgb)
        index += 1

def decode_message_from_image(filepath):
    width, height, pixels = open_image(filepath)
    bits = extract_least_significant_bits(pixels)
    chars = [chr(int(bits[i:i+8], 2)) for i in range(0, len(bits), 8)]
    return ''.join(chars)

def main():
    input_file = "kot.png"
    output_file = "kryptokot.png"
    secret_message = "Hej! Gerwazy! Daj gwintowke! Niechaj strace te makowke!"

    width, height, pixel_data = open_image(input_file)
    message_bits = text_to_binary(secret_message)

    if len(message_bits) > width * height * 3:
        print("Warning: Message is too long to fit in the image, truncating")
        message_bits = message_bits[:width * height * 3]

    embed_message_in_pixels(message_bits, pixel_data)
    write_image(pixel_data, width, height, output_file)

    decoded_message = decode_message_from_image(output_file)[:len(secret_message)]
    print(f"Original Message: {secret_message}")
    print(f"Encoded Message Bits: {message_bits}")
    print(f"Decoded Message: {decoded_message}")

main()
