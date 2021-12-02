# this my proposition for the second part

QueenMoves <- function(n,r_q,c_q){
    if(n <= 0 | any(c(r_q,c_q) <= 0 ) | any(c(r_q,c_q) > n) ){ 
        message("please check the input : \nn must be geater than 0 and the row and column position of the queen (r_q and c_q > 0) can not exceed \nthe dimension of the chessboard n.")
    } else {
        
        up_steps = n - r_q
        down_steps = r_q - 1
        right_steps = n - c_q
        left_steps = c_q - 1
        right_down_steps = n-max(r_q,c_q)
        up_left_steps = abs(1-min(r_q,c_q) )
        dif1 = abs(r_q - n) 
        dif2 = abs(c_q - 1)
        left_down_steps = min(dif1,dif2) 
        dif3 = abs(1 - r_q)
        dif4 = abs(n - c_q)
        right_up_steps = min(dif3,dif4)
        steps = sum(up_steps,down_steps,left_steps,right_steps,right_up_steps,right_down_steps,left_down_steps,up_left_steps)
        table_of_edges = data.frame(rbind( 
            up_edge = c(r_q,c_q) + c(up_steps,0),
            down_edge = c(r_q,c_q) + c(-down_steps,0),
            right_edge = c(r_q,c_q) + c(0,right_steps),
            left_edge = c(r_q,c_q) + c(0,-left_steps),
            right_up_edge = c(r_q,c_q) + c(-right_up_steps,right_up_steps),
            right_down_edge = c(r_q,c_q) + c(right_down_steps,right_down_steps),
            left_up_edge = c(r_q,c_q) + c(-up_left_steps,-up_left_steps),
            left_down_edge = c(r_q,c_q) + c(left_down_steps,-left_down_steps)))
        colnames(table_of_edges) <- c("Row","Column")
        list(up_steps = up_steps , down_steps = down_steps ,
             right_steps = right_steps ,left_steps = left_steps , 
             left_up_steps = up_left_steps , left_down_steps = left_down_steps ,
             right_up_steps = right_up_steps , right_down_steps = right_down_steps,
             total_steps = steps , table_of_edges = table_of_edges) 
    }
}
queensAttack2.0 <- function(n, k, r_q, c_q, obstacles){
    
    if(length(obstacles) == 0){
       result =  QueenMoves(n,r_q,c_q)$total_steps
    } else {
        real_obstacles = data.frame(r = NA , c = NA)
        
        for(i in 1:k){
            
            if(obstacles[i,][1] == r_q){real_obstacles[i,] = obstacles[i,] }
            if(obstacles[i,][2] == c_q){real_obstacles[i,] = obstacles[i,] }
            if( (c(r_q,c_q)-obstacles[i,])[1] == (c(r_q,c_q)-obstacles[i,])[2] ){real_obstacles[i,] = obstacles[i,] }
            if( (c(r_q,c_q)-obstacles[i,])[1] == -((c(r_q,c_q)-obstacles[i,])[2])){real_obstacles[i,] = obstacles[i,] }
        }
        
        real_obstacles = real_obstacles[complete.cases(real_obstacles),]
        real_obstacles = unique(real_obstacles)
        
        up_down_obs = real_obstacles[real_obstacles[,2]==c_q,]
        
        up_obs = up_down_obs[up_down_obs[,1] > r_q,]
        
        up_ob = up_obs[up_obs[,1]==min(up_obs[,1]),]
        
        down_obs = up_down_obs[up_down_obs[,1] < r_q,]
        
        down_ob = down_obs[down_obs[,1]==max(down_obs[,1]),]
        
        left_right_obs = real_obstacles[real_obstacles[,1]==r_q,]
        
        left_obs = left_right_obs[left_right_obs[,2] < c_q,]
        
        left_ob = left_obs[left_obs[,2]==max(left_obs[,2]),]
        
        right_obs = left_right_obs[left_right_obs[,2] > c_q,]
        
        right_ob = right_obs[right_obs[,2]==min(right_obs[,2]),]
        
        diag_obs = real_obstacles[real_obstacles[,1] != r_q & real_obstacles[,2] != c_q , ]
        
        right_down = diag_obs[diag_obs$r > r_q & diag_obs$c > c_q, ]
        left_up = diag_obs[diag_obs$r < r_q & diag_obs$c < c_q, ]
        right_up = diag_obs[diag_obs$r < r_q & diag_obs$c > c_q, ]
        left_down = diag_obs[diag_obs$r > r_q & diag_obs$c < c_q, ]
        
        right_down_ob = right_down[right_down[,2]== min(right_down[,2]) , ]
        left_up_ob = left_up[left_up[,2] == max(left_up[,2]),]
        right_up_ob = right_up[right_up[,2] == min(right_up[,2]),]
        left_down_ob = left_down[left_down[,2] == max(left_down[,2]),]
        result = 0
        if(nrow(up_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$up_steps } else  { 
                result = result +  abs(r_q - up_ob[,1]) - 1 }
        if(nrow(down_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$down_steps } else  { 
                result = result +  abs(r_q - down_ob[,1]) - 1 }
        if(nrow(right_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$right_steps } else  { 
                result = result +  abs(c_q - right_ob[,2]) - 1 }
        if(nrow(left_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$left_steps } else  { 
                result = result +  abs(c_q - left_ob[,2]) - 1 }
        if(nrow(left_up_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$left_up_steps } else  { 
                result = result +  abs(r_q - left_up_ob[,1]) - 1 }
        if(nrow(left_down_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$left_down_steps } else  { 
                result = result +  abs(r_q - left_down_ob[,1]) - 1 }
        if(nrow(right_up_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$right_up_steps } else  { 
                result = result +  abs(r_q - right_up_ob[,1]) - 1 }
        if(nrow(right_down_ob) == 0 ){
            result = result + QueenMoves(n,r_q,c_q)$right_down_steps } else  { 
                result = result +  abs(r_q - right_down_ob[,1]) - 1 }


    }
       

    result
}

# time taken by the largest test case, 100000*100000

time = system.time(queensAttack2.0(n,k,r_q,c_q,obstacles))[3]
time

# elapsed 
# 0.63 
