CPP = g++
FLAGS = -O3
INC = -I../../../

SRC = cmdline.cpp parsers.cpp variables_map.cpp \
	config_file.cpp	positional_options.cpp winmain.cpp \
	convert.cpp utf8_codecvt_facet.cpp \
	options_description.cpp	value_semantic.cpp


boost_program_options.o : $(SRC) 
	$(CPP) -c $(FLAGS) $(INC) $(SRC)
	ld -r *.o -o boost_program_options.o

clean: 
	rm -f  boost_program_options.o *.o
