## SKG
## January 14, 2019
## Taking households of people and changing their location into a grid

hh_to_grid <- function(df, radius = 1){
    new_coords <- plyr::ddply(.var = "HN", .data = df,
                              .fun = function(df){
        N <- nrow(df)
        base_x <- df$x.loc[1]
        base_y <- df$y.loc[1]
        spots_x <- matrix(rep(c(base_x - radius,
                                base_x, base_x + radius), 3),
                          nrow = 3, byrow = TRUE)
        spots_y <- matrix(rep(c(base_y + radius,
                                base_y, base_y - radius), 3),
                          nrow = 3, byrow = FALSE)

        coords_df <- data.frame(x.loc = 1:N, y.loc = 1:N)
        
        if(N == 1){
            coords_df$x.loc <- c(spots_x[2,2])
            coords_df$y.loc <- c(spots_y[2,2])

        } else if( N == 2){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[3,3])
        } else if( N == 3){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[2,2],
                                 spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[2,2],
                                 spots_y[3,3])

        } else if( N == 4){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[1,3],
                                 spots_x[3,1], spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[1,3],
                                 spots_y[3,1], spots_y[3,3])


        } else if( N == 5){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[1,3],
                                 spots_x[2,2],
                                 spots_x[3,1], spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[1,3],
                                 spots_y[2,2],
                                 spots_y[3,1], spots_y[3,3])


        } else if( N == 6){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[2,1],
                                 spots_x[3,1],
                                 spots_x[1,3], spots_x[2,3],
                                 spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[2,1],
                                 spots_y[3,1],
                                 spots_y[1,3], spots_y[2,3],
                                 spots_y[3,3])


        } else if( N == 7){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[2,1],
                                 spots_x[3,1],
                                 spots_x[2,2],
                                 spots_x[1,3], spots_x[2,3],
                                 spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[2,1],
                                 spots_y[3,1],
                                 spots_y[2,2],
                                 spots_y[1,3], spots_y[2,3],
                                 spots_y[3,3])


        } else if( N == 8){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[2,1],
                                 spots_x[3,1],
                                 spots_x[1,2], spots_x[2,2],
                                 spots_x[3,2],
                                 spots_x[1,3],
                                 spots_x[2,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[2,1],
                                 spots_y[3,1],
                                 spots_y[1,2], spots_y[2,2],
                                 spots_y[3,2],
                                 spots_y[1,3],
                                 spots_y[2,3])
            


        } else if( N == 9){
            coords_df$x.loc <- c(spots_x[1,1], spots_x[1,2],
                                 spots_x[3,1],
                                 spots_x[2,2], spots_x[2,1],
                                 spots_x[2,3],
                                 spots_x[1,3], spots_x[2,3],
                                 spots_x[3,3])
            coords_df$y.loc <- c(spots_y[1,1], spots_y[1,2],
                                 spots_y[3,1],
                                 spots_y[2,2], spots_y[2,1],
                                 spots_y[2,3],
                                 spots_y[1,3], spots_y[2,3],
                                 spots_y[3,3])


        }

        return(coords_df)
                              })
    return(new_coords)
}
