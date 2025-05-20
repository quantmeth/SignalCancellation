data = csvread('D:\Documents\R\SignalCancellation\test\N1000_1.csv');
R=data;
R=cov(data);
N=size(data,1);
