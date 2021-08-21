# site는 범죄 현장 정보를 담은 데이터 프레임
site = data.frame(site=c(1:10),
                  x=c(2,5,5,5,1,5,7,4,5,6), 
                  y=c(0,4,1,2,8,9,5,2,4,1))

# seller는 과거 범죄 횟수 정보를 담은 데이터 프레임                  
seller = data.frame(col1=c(1,4,8),
                    col2=c(5,7,1),
                    col3=c(5,1,3))
row.names(seller)=c("Axy","Bxy","Cxy")

site = as.matrix(site)
seller = as.matrix(seller)

# 맨하탄 거리 공식을 이용해서 거래상과 범죄현장의 거리를 계산하는 함수 생성
distance = function(seller, site) {
  dist_mat = matrix(0,10,1)
  for(i in 1:10) {
    temp = abs(seller[1]-site[i,1])+abs(seller[2]-site[i,2])
    dist_mat[i,1] = temp
  }
  return(dist_mat)
}

# dist는 거리를 범죄 이력으로 나눈 값을 저장한 데이터 프레임
dist = data.frame(A=distance(seller[1,1:2], site[,2:3])/seller[1,3],
                  B=distance(seller[2,1:2], site[,2:3])/seller[2,3],
                  C=distance(seller[3,1:2], site[,2:3])/seller[3,3])
dist = as.matrix(dist)

# which.min()을 사용하여 최소값을 갖는 거래상을 할당하는 함수 생성
min_dist = function(dist){
  dis_mat = matrix(0,10,1)
  for(i in 1:10) {
    t = which.min(dist[i,])
    dis_mat[i,1] = t
  }
  return(dis_mat)
}
min_dist(dist)
