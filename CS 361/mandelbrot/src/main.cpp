/**
 @mainpage CS361 - Mandelbrot Homework
 @section Description

 Thread Mandelbrot Image Generation

 Make Commands:

     make

 will build the executable.

     make run

 will run the experiments.

     make clean

 will clear out the compiled code.

     make doc

 will build the doxygen files.

*/

/**
 @file
 @author
 @date 10/23/24
 @section Description

 The main program creates a manager thread, which in turn creates worker threads by dividing the total image into various sections.
*/

#include <iostream>
#include <chrono>
#include <string>
#include <thread>
#include <vector>
#include <cstdlib>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <complex>
#include <algorithm>
#include <mutex>
#include <condition_variable>
// Custom Libraries
#include "bmp.h"
#include "pixel.h"

#define WIDTH 1500
#define THRESHOLD 100

// Global Variables
long double x1_val;
long double x2_val;
long double y1_val;
long double y2_val;
int height;

// Mutex and condition variable for synchronization
std::mutex mtx;
std::condition_variable cv;
bool ready = false;

/**
 * Computes the iteration count for a given point in the Mandelbrot set.
 * @param x Real part of the complex number.
 * @param y Imaginary part of the complex number.
 * @param t Maximum number of iterations.
 * @return The iteration count when the sequence exceeds the threshold, or t if it does not.
 */
int z(long double x, long double y, int t) {
    std::complex<long double> c(x, y);
    std::complex<long double> current(0, 0);
    for (int i = 1; i < t; i++) {
        current = current * current + c;
        if (std::abs(current) > 4) {
            return i;
        }
    }
    return t;
}

/**
 * Converts a C-string to a long double, with error checking.
 * @param str The C-string to convert.
 * @return The converted long double, or NaN on failure.
 */
long double convertToLongDouble(const char* str) {
    char* endPtr;
    errno = 0;
    long double result = std::strtold(str, &endPtr);

    if (errno == ERANGE) {
        std::cerr << "Error: Number out of range for long double." << std::endl;
        return std::nan("");
    }
    if (*endPtr != '\0') {
        std::cerr << "Error: Invalid input. The argument is not a valid number." << std::endl;
        return std::nan("");
    }

    return result;
}

/**
 * Worker thread function to generate a section of the Mandelbrot image.
 * @param start Starting index in the pixel array.
 * @param stop Ending index in the pixel array.
 * @param img Pointer to the bitmap image.
 */
void workerThreadFunction(int start, int stop, bitMap* img) {
    // Wait until the manager thread signals readiness
    {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, [] { return ready; });
    }

    // Generate Mandelbrot section
    for (int i = start; i <= stop; i++) {
        int x = i % WIDTH;
        int y = i / WIDTH;
        long double _x = x1_val + (static_cast<long double>(x) / WIDTH * (x2_val - x1_val));
        long double _y = y1_val + (static_cast<long double>(y) / height * (y2_val - y1_val));

        int value = z(_x, _y, THRESHOLD);
        int rounded = static_cast<int>(std::round(static_cast<double>(value) / THRESHOLD * 255));
        unsigned char shade = static_cast<unsigned char>(rounded);
        pixel p(shade, shade, shade);

        img->setColor(i, p);
    }
}

/**
 * Manager thread function that creates and manages worker threads.
 * @param img Pointer to the bitmap image.
 */
void managerThreadFunction(bitMap* img) {
    // Determine the number of worker threads to use
    unsigned int numThreads = std::thread::hardware_concurrency();
    if (numThreads == 0) {
        numThreads = 1; // Fallback to a single thread if hardware concurrency can't be determined
    }
    numThreads = std::min(numThreads, static_cast<unsigned int>(img->numPixels()));

    // Calculate the step size for each worker thread
    unsigned int step = img->numPixels() / numThreads;

    // Create and launch worker threads
    std::vector<std::thread> workerThreads;
    for (unsigned int i = 0; i < numThreads; i++) {
        int startPoint = i * step;
        int stopPoint = (i + 1) * step - 1;
        if (i + 1 == numThreads) {
            stopPoint = img->numPixels() - 1; // Ensure the last thread processes any remaining pixels
        }
        workerThreads.emplace_back(workerThreadFunction, startPoint, stopPoint, img);
    }

    // Signal worker threads to start processing
    {
        std::lock_guard<std::mutex> lock(mtx);
        ready = true;
    }
    cv.notify_all();

    // Wait for all worker threads to finish
    for (auto& t : workerThreads) {
        t.join();
    }
}

/**
 * Main function to run the Mandelbrot image generation.
 * @param argc Argument count.
 * @param argv Argument vector.
 * @return Exit status.
 */
int main(int argc, char** argv) {
    if (argc != 6) {
        std::cout << "Usage: "
                  << argv[0] << " [x1] [x2] [y1] [y2] [filename]" << std::endl;
        std::cout << "x1, x2, y1, y2 should be numbers defining the view window." << std::endl;
        return 1;
    }

    // Convert command-line arguments to long doubles
    x1_val = convertToLongDouble(argv[1]);
    x2_val = convertToLongDouble(argv[2]);
    y1_val = convertToLongDouble(argv[3]);
    y2_val = convertToLongDouble(argv[4]);

    // Check for conversion errors
    if (std::isnan(x1_val) || std::isnan(x2_val) || std::isnan(y1_val) || std::isnan(y2_val)) {
        std::cerr << "Error: Invalid input values." << std::endl;
        return 1;
    }

    // Ensure x1 < x2 and y1 < y2
    if (x1_val >= x2_val) {
        std::cerr << "Error: x1 should be less than x2." << std::endl;
        return 1;
    }
    if (y1_val >= y2_val) {
        std::cerr << "Error: y1 should be less than y2." << std::endl;
        return 1;
    }

    // Calculate image height while maintaining aspect ratio
    height = static_cast<int>(WIDTH * ((y2_val - y1_val) / (x2_val - x1_val)));
    if (height <= 0) {
        std::cerr << "Error: Computed height is invalid." << std::endl;
        return 1;
    }

    // Create bitmap image
    bitMap* image = new bitMap(WIDTH, height);

    // Create and start the manager thread
    std::thread managerThread(managerThreadFunction, image);

    // Wait for the manager thread to finish
    managerThread.join();

    // Save the image
    std::string filename(argv[5]);
    image->saveImage(filename);

    delete image; // Clean up the allocated memory
    return 0;
}

