/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date 2021-2022
 @section Description

 Implementation of the Bitmap Class
*/
#include <fstream>
#include <string>

#include "pixel.h"
#include "bmp.h"

//Create a new bitmap object
bitMap::bitMap(int w, int h){
    //Set Width and Height
    width = w;
    height = h;
    //Make the Header
    makeHeader();
    //Make the DIB
    makeDIB();
    //Color Array
    imageData = new pixel[numPixels()];
}

//Delete the Bitmap
//We only need to delete things
//created with new
bitMap::~bitMap(){
    delete[] header;
    delete[] DIB;
    delete[] imageData;
}

//Determine the Number of pixels
int bitMap::numPixels(){
    return width*height;
}

//Set a Pixel to a color
void bitMap::setColor(int pos, pixel color){
    //Check Bounds
    if(pos < 0 || pos > numPixels()){
        return;//Can't set
    }
    imageData[pos] = color;
}

/* Save the Image to a File*/
void bitMap::saveImage(std::string filename){
    //Open File
    std::ofstream image(filename,std::ios::binary);
    image.write(header, headerSize);
    image.write(DIB, sizeDIB);
    //Write all pixels
    char * myPixel = new char[3];
    for(int i=0; i < numPixels(); i++)
    {
        myPixel[2] = imageData[i].getRed();
        myPixel[1] = imageData[i].getGreen();
        myPixel[0] = imageData[i].getBlue();
        image.write(myPixel, 3);
    }
    //Close the image stream
    image.close();
    //Exit
    return;
}

/* File Format Helpers*/

//Make the File Header
void bitMap::makeHeader(){
    //Array to store the header
    header = new char[headerSize];
    //The header is 14 Bytes
    //The DIB is 40 bytes
    int offset = headerSize + 40;
    
    //Compute Values we will need
    //How many pixel does the image have
    int pixels = numPixels();
    //Each Pixel is another 3 bytes
    int totalBits = pixels*3+offset;
    
    //Make the Header
    //First 2 Bytes are BM for bitmap
    header[0] = 'B';
    header[1] = 'M';
    //Next 4 bytes are the total size of the file
    header[2] = getByte(totalBits,0);
    header[3] = getByte(totalBits,1);
    header[4] = getByte(totalBits,2);
    header[5] = getByte(totalBits,3);
    //Next for bits are 0 (reserved for other uses)
    header[6] = 0;
    header[7] = 0;
    header[8] = 0;
    header[9] = 0;
    //Last 4 bytes are offset
    //Where do the pixels start
    header[10] = getByte(offset,0);
    header[11] = getByte(offset,2);
    header[12] = getByte(offset,2);
    header[13] = getByte(offset,3);
    //Exit
    return;
}

//Make the File DIB
void bitMap::makeDIB()
{
    //Convert DPI to Pixels Per Meter
    int resolution = dpiToPPM(dpi);
    //Make array of bytes
    DIB = new char[sizeDIB];
    //Set Values
    //First 4 bytes are header size of this header (40)
    DIB[0] = getByte(40,0);
    DIB[1] = getByte(40,1);
    DIB[2] = getByte(40,2);
    DIB[3] = getByte(40,3);
    //Bitmap Width (4 bytes)
    DIB[4] = getByte(width,0);
    DIB[5] = getByte(width,1);
    DIB[6] = getByte(width,2);
    DIB[7] = getByte(width,3);
    //Height (4 bytes)
    DIB[8] = getByte(height,0);
    DIB[9] = getByte(height,1);
    DIB[10] = getByte(height,2);
    DIB[11] = getByte(height,3);
    //Color Plane (2 bytes) is always 1
    DIB[12] = 1;
    DIB[13] = 0;
    //Color Depth (2 bytes) we are using 24 (three 8 bit colors)
    DIB[14] = getByte(24,0);
    DIB[15] = getByte(24,1);
    //Compression (4 bytes) 0 means none
    DIB[16] = 0;
    DIB[17] = 0;
    DIB[18] = 0;
    DIB[19] = 0;
    //Uncompressed Size (4 bytes)
    //0 because we aren't using compression
    DIB[20] = 0;
    DIB[21] = 0;
    DIB[22] = 0;
    DIB[23] = 0;
    //Horizontal Resolution (4 bytes)
    //Pixel per meter
    DIB[24] = getByte(resolution, 0);
    DIB[25] = getByte(resolution, 1);
    DIB[26] = getByte(resolution, 2);
    DIB[27] = getByte(resolution, 3);
    //Vertical Resolution (4 bytes)
    //Pixel per meter
    DIB[28] = getByte(resolution, 0);
    DIB[29] = getByte(resolution, 1);
    DIB[30] = getByte(resolution, 2);
    DIB[31] = getByte(resolution, 3);
    //Color Pallet (4 bytes)
    //0 means all
    DIB[32] = 0;
    DIB[33] = 0;
    DIB[34] = 0;
    DIB[35] = 0;
    //Number of important colors
    //0 mean all equal
    DIB[36] = 0;
    DIB[37] = 0;
    DIB[38] = 0;
    DIB[39] = 0;
    //Exit the Function
    return;
}

//Get a specific byte out of an integer
//for making the header array
char bitMap::getByte(int value, int byte)
{
    int newValue = value;
    
    unsigned char rem;
    for(int i=0; i <= byte; i++)
    {
        rem = static_cast<unsigned char>( newValue%256 );
        newValue = newValue/256;
    }
    return rem;
}

//Convesion
//x pixels/inches * C inches/meter = y pixels/meter
int bitMap::dpiToPPM(int dpi)
{
    float inchesPerMeter = 39.3701/1;
    float convert = dpi*inchesPerMeter;
    return static_cast<int>(convert);
}
