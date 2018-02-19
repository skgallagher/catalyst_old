/* prototypes */

void find_matching_inds(int Cx, int Cy,
			int Nx, int Ny,
			int Mx, int My,
			double x[][Mx], double y[][My],
			int is_ordered, double eps,
			int matching_inds[]);
double abs_val(double x);
void print_float_1d(int N, double a[]);
void print_float_2d(int M, int N, double a[M][N]);
void print_int_1d(int N, int a[]);
int length_int_inds(int A, int a[]);
void subset_array_d(int N_inds, int Ny, int inds[N_inds],
		    int My,
		    double y[Ny][My],
		    double new_y[N_inds][My]);
