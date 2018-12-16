function demosvd()

A3 = [4,3,5;
      2,5,8;
      3,6,10;
      4,5,11];
      
disp(svd(A3))

A = [-2,1,-5,11,2;
     2,-2, 0, 1,4;
     6,-8, 0, 6,0];
     
disp(svd(A))



end % function
