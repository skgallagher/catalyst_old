#include <iostream>
#include <map>
using namespace std;


int main(){
  std::map<int,int*> mymap;
  int nbrs[] = {3, 7, 9};
  mymap[0] = nbrs;
  cout << mymap[0][0] << ' ';
  cout << mymap[0][1] << ' ';
  cout << mymap[0][2] << ' ';
  cout << '\n';

  return 0;
}
