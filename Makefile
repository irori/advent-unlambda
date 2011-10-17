advent.unl: *.scm parser.g.unl
	gosh -I. advent.scm >advent.unl

parser.g.unl: unlc.scm lib.scm enum.scm parser.scm
	gosh -I. parser.scm >parser.g.unl
