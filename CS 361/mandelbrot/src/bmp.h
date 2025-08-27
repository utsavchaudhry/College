/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date 2021-2022
 @section Description

 This is a class representing a bitmap file.
*/

#ifndef _BMP_H_
#define _BMP_H_

#include "pixel.h"
#include <string>

/**
 This class contains an entire bitmap image.
 */
class bitMap{
private:
	int width;/**< Width of the Image */
	int height;/**< Height of the Image*/
	const int dpi = 150;/**< Dots per inch of the Image*/
	//Arrays Storing Binary Data
	char* header;/**< Storage for the file header section*/
	const int headerSize = 14;/**< Number of Bytes in header section*/
	char* DIB;/**< Storage for the Device Independent Bitmap Information*/
	const int sizeDIB = 40;/**< Number of Bytes in the DIB*/
	pixel* imageData;/**<Array of Pixels in image*/
public:
	/**
	 Create a new bitmap with the given size.
	 @param w is the width of the image
	 @param h is the height of the image
	 */
	bitMap(int w, int h);
    /**
	 Delete the bitmap object
	 */
	~bitMap();
    /**
	 Get the number of pixels in the image
	 */
	int numPixels();
    /**
	 Set the color of a single pixel in the image
	 @param pos is the pixel to set the color for
	 @param color is the color to set the pixel as
	 */
	void setColor(int pos, pixel color);
    /**
	 Save the image to a file
	 @param filename is the name of the file to write the data to
	 */
	void saveImage(std::string filename);
private:
	/**
	 Generate a Bitmap File Header for this Image
	 */
	void makeHeader();
	/**
	 Generate a Bitmap DIB for this image
	 */
	void makeDIB();
	/**
	 Given an integer, get a single byte from it
	 @param value is the original integer
	 @param byte is the byte to read (0-3)
	 @return The byte selected
	 */
	char getByte(int value, int byte);
	/**
	 Convert Dots Per Inch to Pixels Per Meter
	 @param dpi is the resolution in dpi
	 @return the resolution in ppm
	 */
	int dpiToPPM(int dpi);
};

#endif
