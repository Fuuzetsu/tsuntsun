Interacts with tesseract to ease reading of RAW Japanese manga.

The usage is fairly simple. Start the program, passing some temporary
filename to use. Use the provided `scrot` button to take snippets of
parts of your screen containing Japanese text. Once scrot has take the
image, `tesseract` will run on it and the output will be visible in
the UI. Mind that `tesseract` is not the fastest so there may be a
small delay depending on size of the image.

The radio buttons change the modes tesseract operates in. Please read
the tesseract manpage to find out more. Vertical text is the default.

![Sample usage](http://fuuzetsu.co.uk/images/1504470733.png)
