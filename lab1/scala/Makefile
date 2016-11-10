# the following are Scala specific defines
SCALAC = scalac

l1c: FORCE
	${SCALAC} -d out src/top/*.scala src/frontend/*.scala src/backend/*.scala src/util/*.scala

clean:
	rm -rf out/*
#find . -name '*.class' | xargs rm -rf

FORCE: clean
	
