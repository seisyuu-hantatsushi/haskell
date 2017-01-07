GHC=ghc

NAME=jpgedecoder

release: jpgedecoder

SRCS  = main.hs
SRCS += JpegDecoderIntDefs.hs
SRCS += tiff.hs
SRCS += dqt.hs
SRCS += dht.hs
SRCS += startofscan.hs
SRCS += decodeEntropy.hs
SRCS += JpegDecoder.hs
SRCS += ../ByteStream/ByteStreamDecoder.hs
SRCS += ../ByteStream/ByteStream.hs

jpgedecoder: $(SRCS)
	$(GHC) -o $(NAME) $(SRCS)

clean:
	rm *~ $(NAME) *.o *.hi
