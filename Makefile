advent.unl: advent.unls room.unls parser.unls
	./unlc.scm advent.unls >advent.unl

parser.unl: parser.unls parser.g.unls
	./unlc.scm parser.unls >parser.unl

parser.g.unls: parser.scm
	gosh parser.scm >parser.g.unls