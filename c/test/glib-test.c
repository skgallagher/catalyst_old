//ex-ghashtable-3.c
#include <glib.h>
#include <stdio.h>

int compare_ints(gpointer a, gpointer b);
void prt(GArray* a);

int main(int argc, char** argv) {

  int N = 16;
  GArray* a = g_array_sized_new(FALSE, FALSE, sizeof(int), N);
  for (int ii=0; ii < N; ii++){
    int a_one = N - ii;
    g_array_append_val(a, a_one);
    printf("The %dth item is '%d'\n", g_array_index(a, int, ii),
	   g_array_index(a, int, ii));
  }
  printf("There are now %d items in the array\n", a->len);
  printf("The first item is '%d'\n", g_array_index(a, gint*, 0));

  printf("Sorting\n");
  g_array_sort(a, (GCompareFunc)compare_ints);
  prt(a);
 
  return 0;
}

int compare_ints(gpointer a, gpointer b) {
 int* x = (int*)a;
 int* y = (int*)b;
 return *x - *y;
}


void prt(GArray* a) {
 int i;
 printf("Array holds: ");
 for (i = 0; i < a->len; i++)
  printf("%d ", g_array_index(a, int, i));
 printf("\n");
}

/* Comipliation command
gcc `pkg-config --cflags --libs glib-2.0` -o glib-test glib-test.c
Source: https://www.ibm.com/developerworks/linux/tutorials/l-glib/
*/
