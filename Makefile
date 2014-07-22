TARGETC=my-reversi
SRCS=Play.hs Command.hs AI.hs

all: $(TARGETS) $(TARGETC)

$(TARGETC): Client.hs $(SRCS)
	ghc -O --make -o $(TARGETC) Client.hs 


clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
	rm -f $(TARGETS).exe
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
