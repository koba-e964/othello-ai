TARGETC=my-reversi
SRCS=Common.hs Board.hs CBoard.hs Command.hs AI.hs Exam.hs

all: $(TARGETS) $(TARGETC)

$(TARGETC): Client.hs $(SRCS)
	ghc -O3 --make -Wall -o $(TARGETC) Client.hs 


clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
	rm -f $(TARGETS).exe
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
