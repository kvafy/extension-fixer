# extension-fixer

Extension-fixer is a utility that fixes file extensions of incorrectly named
files. This is particularly convenient for multi-media files, if your
media viewer relies on the file extension.

Most and foremost, extension-fixer is an experiment of how difficult it is to write an interactive application performing I/O in Haskell. It's quite fun, really.

## Building and running

The extension-fixer is built as a single executable file using _cabal_ as follows:

    $ cabal build

Synopsis:

    $ ./extension-fixer <some-directory>

## Supported file formats

File format is identified based on well-known magic bytes. Currently only a limited set of file formats is supported:

* images: jpg/jpeg, png, gif, bmp, ico, tiff
* audio: mp3, ogg/oga, wma/asf, mka, midi, flac
* video: avi, mkv/mk3d/webm, 3gp/3g2
* archives: zip/jar/docx/xlsx/pptx/odt/ods/odp/..., rar, gz/tar.gz, tar, 7z, bz2, z/tar.z
