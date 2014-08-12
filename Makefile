TARGETC=my-reversi
BENCHC =bench
SRCS=Common.hs CBoard.hs Command.hs AI.hs Exam.hs Human.hs

all: $(TARGETS) $(TARGETC)

$(TARGETC): Client.hs $(SRCS)
	ghc -O3 --make -Wall -o $(TARGETC) Client.hs 

$(BENCHC) : Bench.hs $(SRCS)
	ghc -O3 --make -Wall -o bench Bench.hs
clean: 
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETC)
	rm -f $(TARGETC).exe
	rm -f $(BENCHC)
	rm -f $(BENCHC).exe

