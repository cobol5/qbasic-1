DEFINT A-Z

'$INCLUDE: 'rellib.bi'

DECLARE SUB InitImageData (FileName$, ImageArray%())
DECLARE SUB MakeImageIndex (ImageArray(), IndexArray())

'Special Thanks to Chris Chadwick for those two routines!!!  ;)

SUB InitImageData (FileName$, ImageArray())
'* InitImageData() subroutine:
'* Initializes an integer array with image data - this must be done before
'* displaying an image using the PUT(graphics) statement. The calling value
'* of FileName$ dictates whether the data should be read directly from an
'* image file or from DATA statements (see below).
'*
'* Parameters:
'*    FileName$ - The name of the image file to load. This must include the
'*                path to the file if it does not reside in the current
'*                directory. If FileName$ is an empty string (""), image
'*                data is read from DATA statements.
'* ImageArray() - Dynamic, integer array to hold the image data.
'*
'* Note: Before calling InitImageData() to initialize images from DATA
'*       statements, use an appropriate RESTORE statement to ensure the
'*       correct DATA statements are read.
'*

    IF FileName$ <> "" THEN
        '***** Read image data from file *****

        'Establish size of integer array required.
        FileNo = FREEFILE
        OPEN FileName$ FOR BINARY AS #FileNo
        Ints = (LOF(FileNo) - 7) \ 2
        CLOSE #FileNo
        REDIM ImageArray(1 TO Ints)

        'Load image data directly into array memory.
        DEF SEG = VARSEG(ImageArray(1))
        BLOAD FileName$, 0
        DEF SEG
    ELSE
        '***** Read image data from DATA statements *****

        'Establish size of integer array required.
        READ IntCount
        REDIM ImageArray(1 TO IntCount)

        'READ image DATA into array.
        FOR N = 1 TO IntCount
            READ X
            ImageArray(N) = X
        NEXT N
    END IF


END SUB

SUB MakeImageIndex (ImageArray(), IndexArray())
'* MakeImageIndex() subroutine:
'* Constructs an image position index for the images held in an image array.
'*
'* Parameters:
'* ImageArray() - Dynamic, integer array holding images to be indexed.
'* IndexArray() - Dynamic, integer array to hold the index for images in
'*                ImageArray().
'*

    'The index will initially be built in a temporary array, allowing
    'for the maximum 1000 images per file.
    DIM Temp(1 TO 1000)
    Ptr& = 1: IndexNo = 1: LastInt = UBOUND(ImageArray)
    DO
        Temp(IndexNo) = Ptr&
        IndexNo = IndexNo + 1

        'Evaluate descriptor of currently referenced image to
        'calculate the beginning of the next image.
        X& = (ImageArray(Ptr&) \ 8) * (ImageArray(Ptr& + 1)) + 4
        IF X& MOD 2 THEN X& = X& + 1
        Ptr& = Ptr& + (X& \ 2)
    LOOP WHILE Ptr& < LastInt

    LastImage = IndexNo - 1

    'Copy the image index values into the actual index array.
    REDIM IndexArray(1 TO LastImage)
    FOR N = 1 TO LastImage
        IndexArray(N) = Temp(N)
    NEXT N

END SUB

