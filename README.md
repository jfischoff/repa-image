#Image Manipulation With repa-devil

Currently this library supported bilinear resampling (resizing). 

##Usage

The package contains a library and application called resizer. 

For usage, call resizer without an arguments, but here is the spew.

resizer - a simple image resizing app
    
    $ resizer
    Usage: resizer (-o|--output OUTPUT) ((-w|--width WIDTH) (-h|--height HEIGHT) | (-p|--percent PERCENT)) INPUT FILE
      Resize an image with a HEIGHT and WIDTH or a PERCENT

    Available options:
      -o,--output OUTPUT       Output file path
      -w,--width WIDTH         The ouput width
      -h,--height HEIGHT       The output height
      -p,--percent PERCENT     The amount to reduce the image as a percent
      
It is way to slow right now. I am still working on learning the black art of Repa optimization. However, you can turn up the threads and then the speed is "ok".
To jam up the threads pass the follow -N to the RTS. Here is a example of using 8 threads.

     resizer +RTS -N8 -RTS -p 0.5 -o tests/output.jpeg tests/bigPic.jpeg