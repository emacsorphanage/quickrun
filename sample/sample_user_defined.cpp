#include <iostream>
#include <vector>
#include <string>

int main (int argc, char *argv[])
{
    std::vector <std::string> lst = { "a", "b", "c", "d" };

    for (auto x : lst){
        std::cout << "[" << x << "]" << std::endl;
    }

    for (auto i = 1; i < argc; i++){
        std::cout << "[" << argv[i] << "]" << std::endl;
    }

    return 0;
}

/*
  Local Variables:
  quickrun-command: ((:command . "g++")
                     (:compile . "%c -std=c++0x -o %n %s")
                     (:exec    . "%n apple orange melon")
                     (:remove  . ("%n")))
  End:
*/
