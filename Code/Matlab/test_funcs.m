%%%test cases

CA_data = [7836  53655 115615 161926  38177  46371;...
            13112 102383 184541 340479 105101  58367;...
            6026 42413 59226 62754 12670 14299]';

%%looking good!        
[s,d,t,Fi,Fj,U,V] = coreCA(CA_data);



PCA_data = [3  6  2  6  2  9  6  5  9  4  7 11  5  4  3  9 10  5  4 10;...
            14  7 11  9  9  4  8 11  5  8  2  4 12  9  8  1  4 13 15  6]';

%%also looking good!
[s,d,t,Fi,Fj,P,Q] = corePCA(PCA_data);