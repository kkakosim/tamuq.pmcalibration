function [Y,Xf,Af] = myNNF2(X,~,~)
%MYNEURALNETWORKFUNCTION neural network simulation function.
%
% Generated by Neural Network Toolbox function genFunction, 09-Jun-2017 19:14:49.
% 
% [Y] = myNeuralNetworkFunction(X,~,~) takes these arguments:
% 
%   X = 1xTS cell, 1 inputs over TS timsteps
%   Each X{1,ts} = 23xQ matrix, input #1 at timestep ts.
% 
% and returns:
%   Y = 1xTS cell of 1 outputs over TS timesteps.
%   Each Y{1,ts} = 3xQ matrix, output #1 at timestep ts.
% 
% where Q is number of samples (or series) and TS is the number of timesteps.

%#ok<*RPMT0>

  % ===== NEURAL NETWORK CONSTANTS =====
  
  % Input 1
  x1_step1_xoffset = [13766.7;9100;5379.17;3116.67;1066.67;404.167;616.667;183.333;91.6667;154.167;108.333;116.667;62.5;83.3333;59.1667;53.3333;29.1667;17.5;16.6667;0.833333;0;0;0];
  x1_step1_gain = [1.59714685685491e-05;2.37247924080664e-05;3.96956691826443e-05;9.44696074740575e-05;0.000183191619349798;0.000259571605617804;0.000163431632829918;0.000287424998243115;0.000505263162149585;0.000345323959629557;0.000446096289437979;0.000511727122535362;0.000837696335078534;0.000577617879029314;0.000293577839441193;0.000341442787325855;0.00136908160568355;0.00170575692963753;0.00107286738238007;0.00145366092354624;0.00305732328289259;0.00480962309388625;0.00424027968884828];
  x1_step1_ymin = -1;
  
  % Layer 1
  b1 = [-3.4664485559545661;0.33885250847003739;-0.38694522279390892;-0.56086400786166901;-0.85305131889574703;0.68883173076974624;-0.20921950437191339;1.0346086002280079;0.88779850458933296;-0.16712714423191419;-0.41758048894068572;1.3883254753278771;0.75850731679480787;-0.42965608210743256;0.51515565686017672;0.38895568483372739;0.10997864637147947;-0.69062934770621653;0.92303575216273648;2.5819505936592151;-1.5151773158107229;-0.58619603503417239;-1.1928645922126861];
  IW1_1 = [0.87305320638484774 1.3864485833385591 0.09463689768018313 0.97253587679125875 1.3307140635177592 -0.35297685932229433 0.47846174394310831 0.91572007546024903 -0.48318377831517018 0.89200423315722044 -0.36658576614142013 -0.35272786859804417 0.70558462966226776 0.40310735513188695 -0.57476355310910887 -2.3722989671817656 1.7923247348691547 1.6009194833959124 -0.8295561779881977 -1.0619742216597259 -0.29397169694540182 0.73157845952195755 1.2307023636552674;0.51458312448970567 -0.59720439410295467 -0.35439721399153912 0.60778505936570681 -0.14672817168729058 0.19153185726888741 -0.29822261716955489 0.30163872528414104 -0.026073705581685466 0.25783196430334748 -0.77351883992102743 0.15224546701082026 -0.12701759850430841 0.26948852444108784 0.1372505284450205 -0.36349832274564964 1.0256928637775267 -0.49873144058710017 0.16228691273304968 0.3572115368903086 -0.56945147622013037 0.089487611379565443 -0.35358299728610421;0.4255092335800803 -0.25825720584941475 0.49315040124916898 -0.790727844503474 0.11927791157013096 0.43620854473288817 0.55446297360882812 0.092177420375909577 -1.287429678708893 -0.20826791371495043 1.1727832953601296 0.42190599889517205 -0.26989232134244778 -0.83565830114714024 -1.8875961861542419 1.5125424085258603 0.92661135313014364 0.14891087297319217 0.38250971275619444 -1.5465709880631597 0.58526072286583575 0.15207214087910428 -0.40926573655367887;0.020855478463154633 -0.27582376998073121 0.2232369970881026 0.056393360478734542 0.14364225482601084 -0.076764423722563646 -0.2187832631010129 0.020795944443141191 0.37960921010147508 -0.26992326016088186 0.12498137392114604 -0.27660792092809661 0.3355792258449839 -0.064269195618617089 0.13079915123275448 -0.14074151108470412 -0.57822904805428488 0.29899518133759129 0.11104501836156933 -0.03305447017211003 -0.36313822154856484 -0.050122041716929557 0.83724066219338167;0.2414533437728327 -0.19855240212516251 -0.91737183246857945 1.0777808964494049 -0.26267209845493256 -0.80570882970674507 0.057630725265627507 0.046225798568201047 0.78805484473349707 0.43821772947755466 -0.93158173516704335 0.019452583525407283 -0.058964552977808428 0.39690446227268472 0.92201391786107811 -1.0226606748130511 -0.18913494794612112 -0.71618298053587126 0.28131183297967105 1.1704807348488619 0.036966890020796728 0.4685168845316629 -1.8043460737266153;-0.12729241529419871 -0.35060194057744731 0.50832468648816942 0.23236482775509842 0.78619365215030323 -0.90407776049923205 0.31898332897829557 -0.62163048812698052 1.6950255433370867 -0.77254403391360049 0.20107752457982009 -0.69620455093040479 -0.38233129907808228 0.04476421369159192 -0.33441798671743389 0.29558753507684721 0.43411161839724566 1.4539907186514842 0.54611708658612246 -2.3858209877030192 0.2288367528680218 0.7713375509680791 -0.1687262454508773;0.34768871175731308 -0.8529081999596928 0.46628824648555467 0.2113170949178893 -0.97560995815732343 -0.57339741054405113 -0.84803789588807155 -1.0307622523845559 1.0122588060882978 -0.9380662111354221 1.1549325530595227 1.6364711544978559 -0.52160849958071509 1.9292831036338691 -1.0448051877920648 -0.44340949277963609 -0.020007322586039294 1.1540081527004256 -1.4756833994096799 1.4982538725469681 0.52812153728436106 -0.38815712569426142 2.8987823887182729;-0.43001944317452256 0.62955228376441652 -0.060227959312909141 -0.06635006418433774 -0.0052181327101556117 -0.74974640884484312 -0.063168850883294575 -1.5488326490277637 0.35859084037840439 0.2495448453601139 2.6027555345216067 0.36925543861702609 -0.46778154201893291 -0.49685081125602693 -0.17700486066467797 0.084516491464367627 0.28784794691223059 -0.94035479321178539 -0.46690906216692518 0.55586222810597241 0.62549646348132903 0.66614648239736129 0.57744358365894244;-0.46363694706171177 -0.44318035869599554 0.53166938382136941 0.15055772666885284 0.30254846027550741 -0.10261457063711119 1.0784168358954771 -0.93230453802633284 1.0254258176420739 -0.40021215543843824 0.14714012847052552 -0.1008732632471914 -0.34808202969600255 0.8055940708822027 -0.50607403184493449 0.71879558806631094 0.34962675265259197 -0.14322153180640146 -0.2709225997993211 -0.89974993919726887 0.47240332677961244 0.80892407183715964 -0.86253275011245478;-0.23246036134203246 -0.53930478145596561 0.32750950976317755 0.029773017065654286 0.30866060093041658 0.40432706273608987 0.34219778664450234 0.17699868567543181 0.30599658427236798 0.041785817330468641 -0.5780985517711793 0.084045645365593599 -0.1897692807954427 0.19541201659881505 -0.19987322962384083 0.1033006349612659 0.70286768071395223 -0.52196268535130663 -0.29543115193636371 0.093998680517065419 0.56847047913083459 1.0229066416030028 -0.73152982942381972;0.76506160449207317 0.15039093632982808 -0.64787447052456659 0.17678658803128297 -0.54573465651655229 -0.13909896486841644 0.44388902664648838 0.60955938704604828 -0.90713041882476653 0.33869875163364038 -0.65161167141973619 0.47447688647512376 -0.028884554201070722 0.64225565429828146 0.5073172451708019 -0.31472788592777368 -0.19741260227634175 -0.060478385537101956 0.31704817961592008 0.27162064930814622 0.23631723331470195 -0.43012479536942227 -1.0625247622003871;1.4463401070574831 -1.0898710124903774 -1.5877952774668838 0.89843888343025868 0.29028414280845916 0.55484289174364942 -1.2463865233513953 1.8929342487329051 -1.2548211852709155 -0.16415267785050508 -0.13070706906614232 0.21336649672765187 0.044009403114064406 -1.0495063928173745 0.41896135790974898 -0.65559430681498221 -0.063943700234270567 -0.63868027704953168 0.049732121362004611 0.92099889916205813 -0.29998694107680041 1.2615617540945094 1.5250023342858365;1.2517819096516798 0.41942832644163935 -1.5855717143515964 -1.3920490909995831 -1.9589255993667469 -1.1999235274485078 0.91024349012291428 -2.5512879413658842 -2.8863895905777852 -0.10811711483737158 0.56266606902254834 0.39576126615363655 -1.4820375973181854 -2.6761704177769374 0.90846623914416735 -0.31572182984963332 -0.19186445243663769 2.097865552126394 -2.411384639293169 1.834097825968924 2.4364106412093309 0.77627230477959863 -0.99359499739768642;-0.039825263492282437 0.59761971713944162 -1.1124920943535139 0.572623082329351 -0.52967221159627653 -0.59849867295662251 0.58837351894059475 -0.027462432653142382 -0.076753538108570996 0.67071085356980087 -0.52839818821966689 0.18106342875915346 -0.30929941892901719 -0.31915632375163105 -0.091505822020705577 0.35495099731385249 0.40205652041697387 -0.94640414028782072 0.77491282608143575 1.171146062693055 1.1131238569831934 0.60604442820944382 -2.9566040199289909;0.19769905933157111 -0.78185174553337355 0.43078758351312579 0.32849270718911489 0.22582347761540328 0.10026863367535577 -0.51345341026246216 0.66118993291350547 0.64312213624905734 -0.69294353140753684 0.14679811173472254 -0.90877613246813704 0.90995593913676132 -0.075534397497021941 0.12809686214030117 -0.35304754069504413 -1.594883241287852 0.58232116617960528 0.14741888845540238 0.015533058619435602 -0.39520304581295407 -0.25440104653945089 2.0001398028616038;1.3785186425814016 -0.95637311188795704 0.99948669526173917 -1.8087313148502724 0.026982253820180796 0.018494846840641392 1.0464983767501594 -0.43911569010675761 -1.3635768178270229 -0.05853344787480394 0.60953597864381737 -1.5069060820830174 0.30098048850544956 -1.2026383912610563 -0.54604619402896304 -0.48346896760816555 -0.099259740082325229 -0.45273266598675715 0.66102519344758726 -1.4627155131762932 0.10013667403902396 -0.39354940471686656 0.024279858422161391;-0.77731066361162438 0.76476935684239333 0.65839500634747206 -0.94418349840492899 0.1195014106210207 -0.026578833077896213 0.19555956713628933 0.018844004868623671 -0.33798315978423032 -0.10839566947290334 0.75592650759313607 -0.089456295324745 0.30484261906793553 -0.37552759140827424 -0.10594295920767285 0.19778944020551978 -1.5524399832834264 0.451466094458289 -0.43042732503747294 0.21900064080158832 1.0636216310357471 -0.33321738413442947 0.59534277776618794;0.50640848169322461 0.10202430822192068 -0.51690202685904119 0.16583770699155984 -0.47632759281542419 0.30652924209287291 0.34000332229595032 0.6573975342696261 -0.85777606686749142 0.31768931619773066 -1.1153359378114074 0.28214980978828819 -0.19788922278676349 0.55640775038948487 -0.29874120307103558 -0.84613826335812681 0.25992204112758582 -0.18932890398917079 0.22118731050889406 0.33987893391080271 0.42654132574880355 -0.28235818019848008 -0.55972148804516741;-0.070307133258383309 -1.8915628017048709 3.3417865368904849 -0.3956121867643051 -0.16346530791042579 -0.37397159046869749 -1.8939338957122227 -0.89875960039263436 0.23449646304322341 0.097765832135000119 1.4480983161733869 -0.34300367009369703 0.66049573180370791 -0.11732974520102778 1.1189078244547179 2.0300313775433918 -1.2739823318064043 0.57303030754671025 0.16110495858270604 2.9315412060691077 0.74577455339422849 -0.69281573854377654 0.37399765788073769;0.58978395516836757 1.069956475474733 -0.7368639098685863 0.55526072131579784 -1.5083440644060591 -0.24848218206072928 -0.97356511398065215 -0.42168789925749955 -1.2953546773857691 -0.88848044161105333 0.7563622809549243 0.83734668512028432 0.07083221556014091 -0.36721338675370507 0.16183882917424777 -0.76995017095600626 -2.0810232626596847 -0.30024093874032876 1.2496147061932343 1.4996288268984961 -1.1386406211029936 1.0228437682453457 -0.20573628213327147;-0.45943067680853039 -0.65725533687560611 -0.60793114869694787 -0.2095590156956913 0.21665301183659719 -0.76282390098827224 0.50776476851498531 -0.25891057081801438 -0.43601717491066194 -0.13120359312254726 0.045970273181861906 0.67262580727438848 0.30700412392376031 -0.28637714230591615 -0.55657663467468477 -0.65579547809338889 -1.0133303402428726 -0.57325168598375176 -1.6808348032857561 1.3791882619819316 0.36648245833882798 0.29748671918549524 0.11422013619978227;0.68849574401771663 1.5957491832670203 -0.84191006385721046 0.16373454953840494 -1.1139812678426264 -1.1042646905814932 2.9741266376639155 -0.55613076914299153 -0.48486472905878725 -0.36803674447124801 1.1451678145746427 0.39125204851100392 -1.018966517584712 -0.74495261911614441 1.9383251044390926 -1.4855950759710861 2.7663112884158463 -1.3233050917943958 1.0241196263006134 -1.8636211109803558 0.97953048003093501 0.98740896350213136 0.46200823033805327;-1.0743002843676828 -0.31395604923008402 1.2357670638730069 0.29525315723224949 -0.73582013869648699 1.0580791697473482 -0.88502709830863002 -1.8439186521886934 0.50496838212368056 1.5503105124780994 0.92703926951674109 -1.4753623441341781 -0.60400877905902717 -0.26579932163260234 0.91163027312956135 0.54860326456254582 0.12003669246157496 2.1370649426233643 -0.79551940939393184 0.0085072978806028793 -0.24015095882206522 -0.34780965093444194 0.24913421244144743];
  
  % Layer 2
  b2 = [0.5795234186667173;-0.16535366999846413;0.14324332303429799];
  LW2_1 = [0.13411318610959702 0.78274128437570323 -0.19107533921223585 1.7561734051394706 -0.4614978572215489 0.11592201457960205 0.015045895376333548 0.10323060531828622 0.11327717662503871 0.1913063029842394 0.20053812392902781 0.067999110326016229 -6.1025637153796228e-05 0.28743098383879967 -0.19660717721016521 -0.0033891560810967972 0.37622353676902104 0.042943816466268436 -0.080330049685062008 0.021845983306217651 0.007716507636833396 0.18070676474585445 0.16130645651284145;0.26629128476626679 0.83676944414820487 -0.20201323559990353 0.4583824338110391 -0.46867398852073477 0.096162804457779569 0.0058632211149214387 0.021193305808773509 0.11609340382044715 0.11521456539334476 0.49793225143707387 0.025082597797718816 -0.006967705672453881 0.22830035359118675 -0.026810728892874558 -0.00066991992804991572 0.28072288353236413 -0.42256261495548531 -0.015621356843688667 -0.020490281202897767 -0.005103527938925761 0.07097624554795437 0.10815136103533809;0.23476035856798602 0.22109156726072762 0.43222271211431096 -0.35315895292228244 0.41604529707418147 0.066983797931595784 -0.027380820196887888 0.37108352964538555 -0.040884984442995195 0.35126440767430833 0.84175825229199974 -0.14514503044827559 0.032773500538421513 -0.26591021703518203 0.83926072076652192 0.11264716450502871 -0.28379982545070642 -0.0099663564988069567 0.016379884473759144 -0.032485616688256884 -0.18984704224167676 0.0043098169807122277 0.030695641554244753];
  
  % Output 1
  y1_step1_ymin = -1;
  y1_step1_gain = [0.00095128018530938;0.0104357080291346;0.123013864277573];
  y1_step1_xoffset = [23.7;6.68333;2.59167];
  
  % ===== SIMULATION ========
  
  % Format Input Arguments
  isCellX = iscell(X);
  if ~isCellX, X = {X}; end;
  
  % Dimensions
  TS = size(X,2); % timesteps
  if ~isempty(X)
    Q = size(X{1},2); % samples/series
  else
    Q = 0;
  end
  
  % Allocate Outputs
  Y = cell(1,TS);
  
  % Time loop
  for ts=1:TS
  
    % Input 1
    Xp1 = mapminmax_apply(X{1,ts},x1_step1_gain,x1_step1_xoffset,x1_step1_ymin);
    
    % Layer 1
    a1 = tansig_apply(repmat(b1,1,Q) + IW1_1*Xp1);
    
    % Layer 2
    a2 = repmat(b2,1,Q) + LW2_1*a1;
    
    % Output 1
    Y{1,ts} = mapminmax_reverse(a2,y1_step1_gain,y1_step1_xoffset,y1_step1_ymin);
  end
  
  % Final Delay States
  Xf = cell(1,0);
  Af = cell(2,0);
  
  % Format Output Arguments
  if ~isCellX, Y = cell2mat(Y); end
end

% ===== MODULE FUNCTIONS ========

% Map Minimum and Maximum Input Processing Function
function y = mapminmax_apply(x,settings_gain,settings_xoffset,settings_ymin)
  y = bsxfun(@minus,x,settings_xoffset);
  y = bsxfun(@times,y,settings_gain);
  y = bsxfun(@plus,y,settings_ymin);
end

% Sigmoid Symmetric Transfer Function
function a = tansig_apply(n)
  a = 2 ./ (1 + exp(-2*n)) - 1;
end

% Map Minimum and Maximum Output Reverse-Processing Function
function x = mapminmax_reverse(y,settings_gain,settings_xoffset,settings_ymin)
  x = bsxfun(@minus,y,settings_ymin);
  x = bsxfun(@rdivide,x,settings_gain);
  x = bsxfun(@plus,x,settings_xoffset);
end