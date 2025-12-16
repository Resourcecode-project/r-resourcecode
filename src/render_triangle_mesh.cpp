#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rasterize_triangles(NumericMatrix tri_mat, NumericVector x,
                                 NumericVector y, NumericVector z,
                                 int nx=500, int ny=500,
                                 bool draw_edges=false) {

  double xmin = min(x), xmax = max(x);
  double ymin = min(y), ymax = max(y);

  NumericMatrix gridZ(ny, nx); // rows=ny, cols=nx, matches image()
  gridZ.fill(NA_REAL);

  NumericVector triX, triY;
  if(draw_edges){
    triX = NumericVector(tri_mat.ncol()*3);
    triY = NumericVector(tri_mat.ncol()*3);
  }
  int count=0;

  for(int t=0; t<tri_mat.ncol(); t++){
    int i1 = tri_mat(0,t)-1;
    int i2 = tri_mat(1,t)-1;
    int i3 = tri_mat(2,t)-1;

    double x1=x[i1], y1=y[i1], z1=z[i1];
    double x2=x[i2], y2=y[i2], z2=z[i2];
    double x3=x[i3], y3=y[i3], z3=z[i3];

    if(draw_edges){
      triX[count] = x1; triY[count] = y1; count++;
      triX[count] = x2; triY[count] = y2; count++;
      triX[count] = x3; triY[count] = y3; count++;
    }

    // Bounding box in grid coordinates
    int ixmin = std::max(0, (int)std::floor((std::min({x1,x2,x3}) - xmin)/(xmax-xmin)*(nx-1)));
    int ixmax = std::min(nx-1, (int)std::ceil((std::max({x1,x2,x3}) - xmin)/(xmax-xmin)*(nx-1)));
    int iymin = std::max(0, (int)std::floor((std::min({y1,y2,y3}) - ymin)/(ymax-ymin)*(ny-1)));
    int iymax = std::min(ny-1, (int)std::ceil((std::max({y1,y2,y3}) - ymin)/(ymax-ymin)*(ny-1)));

    for(int ix=ixmin; ix<=ixmax; ix++){
      double px = xmin + (xmax-xmin)*ix/(nx-1);
      for(int iy=iymin; iy<=iymax; iy++){
        double py = ymin + (ymax-ymin)*iy/(ny-1);

        // barycentric coordinates
        double detT = (y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3);
        double l1 = ((y2 - y3)*(px - x3) + (x3 - x2)*(py - y3)) / detT;
        double l2 = ((y3 - y1)*(px - x3) + (x1 - x3)*(py - y3)) / detT;
        double l3 = 1.0 - l1 - l2;

        if(l1 >=0 && l2>=0 && l3>=0){
          gridZ(ny-1-iy, ix) = l1*z1 + l2*z2 + l3*z3; // flip y for image()
        }
      }
    }
  }

  return List::create(
    Named("gridZ") = gridZ,
    Named("xmin") = xmin,
    Named("xmax") = xmax,
    Named("ymin") = ymin,
    Named("ymax") = ymax,
    Named("triX") = triX,
    Named("triY") = triY
  );
}
