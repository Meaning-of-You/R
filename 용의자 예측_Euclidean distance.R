# site는 범죄 현장 정보를 담은 매트릭스
site = matrix(c(2,5,5,5,1,5,7,4,7,6,0,4,1,2,8,9,5,2,4,1),ncol=2)

 # 유클리디안 거리 공식을 이용해서 거래상과 범죄현장의 거리를 계산
dist <- dist(site, method="euclidean")
plot(site)
colMeans(site)
criminal=c(4.5,3.6)

# 범죄자의 위치
points(4.5,3.6,col='red')

Euclidean_distance=matrix(0,10,1) # 0으로 채워진 10행 1열의 매트릭스 생성

# 두 점의 유클리디안 거리 = 제곱근( (x2-x1)^2 + (y2-y1)^2 )
for(i in 1:10){
  Euclidean_distance[i,1]=sqrt(t(as.matrix(site[i,]-criminal))%*%as.matrix(site[i,]-criminal))                         
}