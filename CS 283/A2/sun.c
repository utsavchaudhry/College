#include <stdio.h>
#include <stdlib.h>

void read_pgm_header(FILE *file, int *width, int *height, int *max_val) {
    fscanf(file, "P5 %d %d %d", width, height, max_val);
    fgetc(file); // Consume the newline character after the max_val
}

void read_pgm_data(FILE *file, unsigned char *data, int size) {
    fread(data, sizeof(unsigned char), size, file);
}

void write_pgm(const char *filename, unsigned char *data, int width, int height, int max_val) {
    FILE *file = fopen(filename, "wb");
    fprintf(file, "P5\n%d %d\n%d\n", width, height, max_val);
    fwrite(data, sizeof(unsigned char), width * height, file);
    fclose(file);
}

void locate_sun(unsigned char *data, int width, int height, int *min_x, int *max_x, int *min_y, int *max_y) {
    *min_x = width;
    *max_x = 0;
    *min_y = height;
    *max_y = 0;
    for (int y = 0; y < height - 40; y++) { // Ignore the bottom 40 rows
        for (int x = 0; x < width; x++) {
            if (data[y * width + x] > 128) { // Assuming sun pixels are bright
                if (x < *min_x) *min_x = x;
                if (x > *max_x) *max_x = x;
                if (y < *min_y) *min_y = y;
                if (y > *max_y) *max_y = y;
            }
        }
    }
}

void extract_subimage(unsigned char *input, unsigned char *output, int width, int height, int min_x, int max_x, int min_y, int max_y) {
    int sun_center_x = (min_x + max_x) / 2;
    int sun_center_y = (min_y + max_y) / 2;

    int start_x = sun_center_x - 250;
    int start_y = sun_center_y - 250;

    for (int y = 0; y < 500; y++) {
        for (int x = 0; x < 500; x++) {
            int src_x = start_x + x;
            int src_y = start_y + y;
            if (src_x >= 0 && src_x < width && src_y >= 0 && src_y < height) {
                output[y * 500 + x] = input[src_y * width + src_x];
            } else {
                output[y * 500 + x] = 0; // Fill with black if out of bounds
            }
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s input.pgm output.pgm\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "rb");
    if (!input_file) {
        perror("Error opening input file");
        return 1;
    }

    int width, height, max_val;
    read_pgm_header(input_file, &width, &height, &max_val);

    int size = width * height;
    unsigned char *input_data = (unsigned char *)malloc(size * sizeof(unsigned char));
    read_pgm_data(input_file, input_data, size);
    fclose(input_file);

    int min_x, max_x, min_y, max_y;
    locate_sun(input_data, width, height, &min_x, &max_x, &min_y, &max_y);

    unsigned char *output_data = (unsigned char *)malloc(500 * 500 * sizeof(unsigned char));
    extract_subimage(input_data, output_data, width, height, min_x, max_x, min_y, max_y);

    write_pgm(argv[2], output_data, 500, 500, max_val);

    free(input_data);
    free(output_data);

    return 0;
}
