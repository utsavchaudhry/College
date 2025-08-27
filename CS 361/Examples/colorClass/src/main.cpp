/**
 @mainpage
 @section Description
 
This program makes a bitmap with a random color.
 */

/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date 2021-2022
 @section Description
 
 This file parsed the command line input and starts threads to color the pixels.
 */

//Standard Libraries
#include <iostream>
#include <string>
#include <thread>

//Custom Libraries
#include "bmp.h"
#include "pixel.h"

/**
 Color Every Pixel in a section (start to stop) a color.
 @param img is the bitmap
 @param start is the first pixel to color
 @param stop is the last pixel to color
 */
void colorThread(bitMap* img,
    int start,
    int stop,
    pixel color);
/**
 Create an Image of a given size and store it in a file.
 @param argc number of command line arguments, expected value is 4
 @param argv has values argv[1] x size, argv[2] y size, and argv[3] is the filename to save
 @return 1 on failure and 0 on success
 */
int main(int argc, char** argv)
{
    //Get Height and Width From Command Line
    if(argc!=4){
        std::cout << "bmp xSize ySize filename"
            << std::endl;
        return 1;
    }
    //Initiate a random number generator
    std::srand(std::time(NULL));
    //Determine command line args
    int width = atoi(argv[1]);
    int height = atoi(argv[2]);
    std::string filename(argv[3]);
    //Start up
    std::cout << "Generating "
        << width << " by "
        << height << " image."
        << std::endl;
    //Use the classes to make the image
    //We are making a pointer to the image
    bitMap* image = new bitMap(width,height);
    
    //Color In the Image
    //Use the hardware Number of threads
    unsigned int numThreads
        = std::thread::hardware_concurrency();
    //Determine Step Size
    unsigned int step = image->numPixels()/numThreads;
    //Array of Threads we are running
    std::thread* myThreads = new std::thread[numThreads];
    //Start all Threads
    for(int i=0; i < numThreads; i++){
        //Determine when to start
        int startPoint = i*step;
        //Determine when to stop
        int stopPoint = (i+1)*step-1;
        //Make sure we reach end
        if(i+1==numThreads){
            stopPoint=image->numPixels()-1;
        }
        //Determine Random Color
        unsigned char red = std::rand()%256;
        unsigned char green = std::rand()%256;
        unsigned char blue = std::rand()%256;
        //Create pixel to use
        pixel p(red,green,blue);
        //Print for Testing
        std::cout << "Thread for "
            << startPoint
            << " to "
            << stopPoint
            << std::endl;
        //Create Thread
        myThreads[i] = std::thread(
            colorThread,//Function to run
            image, //Class to modify
            startPoint, //Start point
            stopPoint, //Stop Point
            p //Color to use
            );
    }
    //Join all Threads
    for(int i=0; i < numThreads; i++){
        myThreads[i].join();
    }
    
    
    //Save the image
    //This is a pointer, which means
    //We use -> to access the method
    //instead of .
    image->saveImage(filename);
    
	return 0;
}

//Colors every pixel between
//start and stop (inclusive)
//with color given
void colorThread(bitMap* img,
    int start, int stop,
    pixel color){
    //Color In the Image
    for(int i=start; i <= stop; i++){
        img->setColor(i,color);
    }
}
