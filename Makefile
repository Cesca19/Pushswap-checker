##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## to compile project
##

SRC	=	$(shell find $(SRC_DIR) -name '*.hs')

OBJ	=	$(SRC:.lhs=.o)

NAME	=	pushswap_checker

GXX	=	ghc

all:	$(NAME)

$(NAME):	$(OBJ)
	@$(GXX) -o $(NAME) $(OBJ)

clean:
	@rm -f *.o
	@rm -f *~
	@rm -f *.hi

fclean:		clean
	@rm -f $(NAME)

re:	fclean all