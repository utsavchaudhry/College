/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date 2021-2022
 @section Description
 
 A class representing a single pixel. It contains Red, Green, and Blue Components
 */


#ifndef _PIXEL_H_
#define _PIXEL_H_
/**
 This class represents one pixel in an image.
 */
class pixel{
private:
	unsigned char red;/**< The value of the red component of the pixel.*/
	unsigned char green;/**< The value of the green component of the pixel.*/
	unsigned char blue;/**< The value of the blue component of the pixel.*/
public:
	/**
		The default constructor creates a black pixel.
	 */
	pixel(){
		red = 0;
		green = 0;
		blue = 0;
	}
    /**
	 This constructor lets the user pick the color.
	 @param r is the red value
	 @param g is the green value
	 @param b is the blue value
	 */
	pixel(unsigned char r,
		  unsigned char g,
		  unsigned char b){
		red = r;
		green = g;
		blue = b;
	}
    /**
	 Get the red color value.
	 @return Value of red color
	 */
	unsigned char getRed(){return red;}
	/**
	 Get the green color value.
	 @return Value of green color
	 */
	unsigned char getGreen(){return green;}
	/**
	 Get the blue color value.
	 @return Value of blue color
	 */
	unsigned char getBlue(){return blue;}
	/**
	 Change the red value.
	 @param r is the new red value
	 */
	void setRed(unsigned char r){red = r;}
	/**
	 Change the green value.
	 @param g is the new red value
	 */
	void setGreen(unsigned char g){green = g;}
	/**
	 Change the blue value.
	 @param b is the new red value
	 */
	void setBlue(unsigned char b){blue = b;}
};

#endif
