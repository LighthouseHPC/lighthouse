# author : Kanika Sood 
# Date : Feb 13

# default timings available for 256 matrices (default solver: gmres + ilu , solver id: 32168839)
#input file: '/Users/kanikas/Desktop/petsc_anamod_35.csv' (File has all features + solver + class)
#output file: '/Users/kanikas/Desktop/solver_pc.csv' (File has all features + solver + solver_name + pc_name + class ) manually removing solver from the list for now
#input for this script is generated from CsvComparison.java

import csv
from collections import OrderedDict
from itertools import islice 
import operator


			
#Open and read the solver unique numbers from solverids_names.csv and make them the key of the dictionary
uniques_ids = {}
default_time = {}

with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RFRS2combinedNoNoise.csv', 'r+') as csvinput:
#with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/combined21505_feb22.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = '/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2default_Time433NoNoise.csv'
		with open(csvoutput,'w') as csvoutput:
			writer = csv.writer(csvoutput)
			header= infile.__next__() 
			#print(header)
			writer.writerow(header + ['default_time']) #write the header to the new decoupled file before writing the feature values
			for row in islice(infile, 1, None):
					if row[6] == "32168839" : #67
						default_time[row[8]] = row[9]
					writer.writerow(row)
			print(default_time)
default_time={'hydr1c_A_76':'0.000341',
'hydr1c_A_27':'0.000184',
'hydr1c_A_26':'0.000185',
'hydr1c_A_25':'0.000222',
'hydr1c_A_23':'0.016615',
'hydr1c_A_22':'0.000432',
'hydr1c_A_21':'0.00035',
'Si5H12':'0.048989',
'g7jac020':'0.24685',
'g7jac140':'449.2588',
'rail_5177_E':'0.000117',
'ex18':'0.000585',
'bcsstm20':'0.000012',
'fpga_dcop_23':'0.15893',
'fpga_dcop_26':'0.15943',
'fpga_dcop_28':'17.19969',
'extr1b_A_15':'0.000181',
'bayer10':'0.000449',
'Pres_Poisson':'0.034215',
'TSOPF_RS_b162_c4':'0.001469',
'S40PI_n':'22.73836',
'mark3jac020sc':'0.084734',
'adder_dcop_04':'0.00013',
'adder_dcop_07':'0.000383',
'adder_dcop_01':'0.073208',
't2d_q9_A_43':'0.006103',
'extr1b_A_12':'0.013618',
'radfr1':'0.000026',
'hydr1c_A_24':'0.000186',
'extr1b_A_11':'0.000106',
'ck400':'0.028677',
't2d_q9_A_45':'0.317482',
'extr1b_A_10':'0.000108',
't2d_q9_A_44':'0.006147',
't2d_q4_A_03':'0.006511',
'hydr1c_A_20':'0.000206',
'jan99jac020sc':'58.17999',
'fd18':'0.000313',
'fd15':'0.009101',
'rdist1':'0.000271',
'rdist2':'0.000128',
'fd12':'0.000068',
't2d_q4_A_11':'0.006502',
'fs_760_2':'0.001147',
'as-caida_G_121':'0.001232',
'as-caida_G_120':'258.1891',
't2d_q4_A_17':'0.006381',
'as-caida_G_122':'0.001217',
't2d_q4_A_19':'0.006317',
't2d_q4_A_18':'0.3368',
'oscil_dcop_56':'0.086988',
'c-39':'0.000231',
'c-38':'0.00016',
't2d_q9_A_03':'0.335155',
'oscil_dcop_57':'0.0899',
'oscil_dcop_41':'0.089111',
'hydr1c_A_17':'0.000185',
'c-31':'0.024532',
'c-30':'0.000115',
'c-33':'0.000119',
'c-32':'0.000105',
'c-35':'0.000128',
'c-34':'0.000193',
'c-37':'0.000154',
'hydr1c_A_15':'0.000186',
'oscil_dcop_52':'0.087516',
'cz20468':'4.1895',
'rail_1357_E':'0.01738',
'oscil_dcop_50':'0.087447',
'oscil_dcop_53':'0.087165',
'hydr1c_A_13':'0.000234',
'oscil_trans_01':'0.020292',
't2d_q4_A_14':'0.006471',
'hydr1c_A_10':'0.029182',
's1rmt3m1':'0.040435',
'west0381':'0.00001',
'hvdc1':'213.4033',
'Trefethen_150':'0.000012',
'oscil_dcop_51':'0.087491',
'std1_Jac2':'0.002412',
'jan99jac040':'0.005207',
'fpga_dcop_20':'0.1578',
'g7jac060sc':'179.4371',
'fpga_dcop_21':'0.15799',
'viscoplastic2_C_4':'6.897766',
'viscoplastic2_C_5':'0.17968',
'viscoplastic2_C_3':'0.13963',
'LF10000_E':'0.000261',
'c-48':'0.300205',
'c-49':'0.000412',
'c-44':'0.000189',
'c-45':'0.000356',
'c-46':'0.000266',
'c-47':'0.000353',
'c-40':'0.000195',
'c-41':'0.000234',
'c-42':'0.000209',
'circuit_2':'0.000843',
'ex21':'0.003923',
'fpga_dcop_27':'0.158',
'bodyy6':'0.000354',
'extr1b_A_04':'0.000112',
'extr1b_A_05':'0.013844',
'extr1b_A_06':'0.00021',
'extr1b_A_07':'0.000358',
'extr1b_A_01':'0.000202',
'extr1b_A_02':'0.000204',
'extr1b_A_03':'0.000214',
'extr1b_A_08':'0.00018',
'extr1b_A_09':'0.000181',
'lung1':'0.02021',
'ex26':'1.398',
'cavity20':'0.69697',
'cavity21':'24.50512',
'cavity22':'0.96544',
'cavity23':'1.0435',
'cavity24':'1.2904',
'cavity25':'1.4848',
'fpga_dcop_10':'0.15807',
'oscil_dcop_08':'11.57782',
'g7jac020sc':'55.05309',
't2d_q4_A_24':'0.006635',
's2rmq4m1':'0.000479',
'cyl6':'0.033528',
'TSOPF_RS_b162_c1':'0.00027',
'fpga_dcop_14':'0.15906',
'adder_dcop_41':'0.000504',
'adder_dcop_43':'0.019027',
'adder_dcop_42':'0.000507',
'adder_dcop_45':'0.000312',
'adder_dcop_44':'0.000269',
'ted_B_unscaled':'0.000295',
'raefsky3':'0.18363',
'adder_dcop_48':'0.000499',
'raefsky1':'0.007827',
'onetone1':'0.000785',
'raefsky6':'0.001105',
'raefsky5':'0.036347',
'raefsky4':'0.003524',
'west0132':'0.000007',
'circuit204':'0.011021',
'nasa4704':'0.00021',
'g7jac010sc':'0.184038',
'lhr17c':'0.001003',
'hydr1c_A_70':'0.016711',
'hydr1c_A_71':'0.00021',
'hydr1c_A_72':'0.000277',
'hydr1c_A_73':'0.000184',
'hydr1c_A_74':'0.000186',
'hydr1c_A_75':'0.000209',
'extr1':'0.013004',
't2d_q9_A_18':'0.006322',
't2d_q9_A_19':'0.00645',
'hydr1':'0.000293',
'utm5940':'1.5039',
's3rmt3m1':'0.035763',
'e40r0100':'0.001481',
'rim':'360.0818',
'cz148':'0.000132',
'g7jac050sc':'0.098392',
'LFAT5_E':'0.000009',
'wang3':'0.024596',
'Zd_Jac3':'0.004462',
'vt2010':'0.000436',
'Dubcova1':'0.026835',
'std1_Jac3_db':'0.001126',
'wang4':'0.018522',
'g7jac080':'214.4989',
'bbmat':'0.123987',
't2d_q4_A_43':'0.006026',
't2d_q4_A_41':'0.006019',
't2d_q4_A_44':'0.011432',
'fp':'289.5589',
't2dal_bci_E':'0.000038',
'cz5108':'14.41127',
'oscil_dcop_38':'0.086737',
'oscil_dcop_39':'0.088339',
'west0156':'0.000014',
'chipcool0':'1.073009',
'chipcool1':'0.034948',
'oscil_dcop_32':'0.086821',
'oscil_dcop_33':'0.087493',
'oscil_dcop_30':'0.086866',
'oscil_dcop_31':'0.090486',
'oscil_dcop_36':'0.086962',
'oscil_dcop_37':'0.086477',
'oscil_dcop_34':'11.57676',
'oscil_dcop_35':'0.087309',
'rajat22':'281.7301',
'rajat27':'0.00706',
'poli3':'0.078904',
'poli4':'0.002707',
'lhr04':'0.000125',
'west0167':'0.000009',
'S20PI_n':'16.33091',
'b_dyn':'0.00007',
'jan99jac040sc':'0.069728',
'inlet':'0.000568',
'PGPgiantcompo':'0.000142',
'pores_3':'0.000464',
'hydr1c_A_38':'0.000204',
'hydr1c_A_39':'0.000352',
't2d_q4_A_10':'0.006607',
'hydr1c_A_34':'0.000218',
'hydr1c_A_35':'0.000229',
'hydr1c_A_36':'0.017321',
'hydr1c_A_37':'0.000366',
'hydr1c_A_30':'0.000216',
'hydr1c_A_31':'0.000206',
'hydr1c_A_32':'0.000223',
'hydr1c_A_33':'0.000207',
'heart2':'0.074416',
'ted_A':'6.263',
'oscil_dcop_54':'0.086579',
't2d_q9_A_31':'0.006361',
't2d_q4_A_40':'0.312631',
'fpga_dcop_32':'0.1571',
'fpga_dcop_31':'17.17541',
'fpga_dcop_30':'0.15854',
'fpga_dcop_37':'0.15769',
'fpga_dcop_36':'0.15775',
'fpga_dcop_35':'0.15845',
'fpga_dcop_34':'0.15765',
'impcol_a':'0.000009',
'g7jac140sc':'449.92',
'fpga_dcop_39':'0.15938',
'fpga_dcop_38':'0.15852',
'rail_5177':'0.023114',
'Zd_Jac3_db':'0.001207',
'bayer08':'0.50837',
'bayer09':'0.000141',
'bayer06':'0.50804',
'bayer07':'0.52749',
'bayer04':'0.001025',
'bayer05':'0.5283',
'bayer02':'1.5747',
'bayer03':'0.00074',
'adder_dcop_10':'0.033598',
'adder_dcop_11':'0.000089',
't2d_q4_A_12':'0.006477',
'nasa1824':'0.01111',
't2dah_a':'0.000439',
'hydr1c_A_64':'0.0002',
't2dal_bci_Aside':'32.6453',
'rail_20209_E':'0.036738',
't2d_q9_A_29':'0.318966',
'garon1':'0.425441',
'xingo3012':'0.000704',
'add32':'0.001612',
'hydr1c_A_28':'0.000218',
't2d_q9_A_22':'0.006344',
't2d_q9_A_21':'0.006548',
't2d_q9_A_27':'0.006326',
't2d_q9_A_07':'0.006516',
't2d_q9_A_25':'0.00634',
't2d_q9_A_24':'0.006349',
'g7jac040':'0.6211',
't2d_q4_A_15':'0.00636',
'hydr1c_A_68':'0.000287',
'spiral_E':'0.1371',
'Zd_Jac6_db':'0.001451',
'smt':'0.008338',
't2d_q4_A_13':'0.006342',
't2d_q4_A_06':'0.006604',
't2d_q4_A_07':'0.00648',
't2d_q4_A_05':'0.0066',
't2d_q4_A_02':'0.331075',
'as-caida_G_115':'5.9352',
't2d_q4_A_01':'0.00648',
'as-caida_G_119':'258.412',
'Reuters911_Day_38':'0.00008',
'filter2D':'0.018787',
't2d_q4_A_08':'0.006542',
't2d_q4_A_09':'0.006577',
'cz628':'0.208129',
'flowmeter0':'0.000142',
'LFAT5000_E':'0.024796',
'flowmeter5':'0.724053',
't2d_q4_A_16':'0.006341',
'LFAT5000_M':'0.001364',
'DK01R':'0.020493',
'juba40k':'0.104063',
'piston_M':'20.84722',
'viscoplastic1':'0.012969',
'TSOPF_RS_b9_c6':'0.000166',
'av41092':'0.003284',
'SiNa':'0.000316',
'g7jac060':'150.898',
'oscil_dcop_55':'11.57841',
'S80PI_n':'35.45733',
't2d_q9_A_36':'0.00625',
'rail_20209':'0.000462',
'Si10H16':'0.001817',
'piston_E':'1.0572',
'west2021':'0.000038',
't2d_q9_A_37':'0.006268',
'spiral':'0.017827',
'std1_Jac3':'0.002031',
'west0655':'0.000019',
'c-57':'0.000714',
'c-56':'0.000701',
'c-54':'0.000668',
'c-53':'0.000794',
'c-52':'0.000415',
'c-51':'0.000402',
'c-50':'0.000557',
'extr1b_A_39':'0.000183',
'extr1b_A_38':'0.000189',
'extr1b_A_31':'0.38093',
'extr1b_A_30':'0.000113',
'extr1b_A_33':'0.013579',
'extr1b_A_32':'0.000189',
'extr1b_A_35':'0.000181',
'extr1b_A_34':'0.37655',
'extr1b_A_37':'0.37439',
'extr1b_A_36':'0.000094',
'oscil_dcop_45':'0.088029',
'oscil_dcop_49':'0.087198',
'oscil_dcop_48':'0.086043',
'lhr17':'0.00108',
'lhr10':'0.000331',
'LFAT5000':'0.000306',
'oscil_dcop_44':'0.084981',
'lhr11':'0.000334',
'sinc18':'0.00154',
'sinc12':'0.000591',
'c-36':'0.000137',
'sinc15':'0.024831',
't2dal_bci':'0.012745',
't2d_q4_A_42':'0.006091',
'adder_dcop_52':'0.000843',
'adder_dcop_50':'0.000847',
'adder_dcop_51':'0.028563',
'adder_dcop_56':'0.000303',
'adder_dcop_54':'0.000849',
'cz308':'0.000339',
'mark3jac040sc':'0.262266',
'oscil_dcop_43':'0.088443',
'Zhao2':'85.02454',
'jan99jac020':'0.129045',
'hydr1c_A_45':'0.000223',
'hydr1c_A_44':'0.000369',
'hydr1c_A_47':'0.000291',
'hydr1c_A_46':'0.016937',
'hydr1c_A_41':'0.000186',
'hydr1c_A_43':'0.000203',
'hydr1c_A_42':'0.000204',
'ex13':'0.021507',
'hydr1c_A_49':'0.000184',
'hydr1c_A_48':'0.000205',
'fpga_dcop_48':'17.17434',
'plat1919':'0.021004',
'fpga_dcop_41':'0.1571',
'fpga_dcop_46':'0.15783',
'lhr34c':'0.001476',
'ex7':'0.00174',
'Zd_Jac2':'0.003849',
'ex5':'0.013775',
'Zd_Jac6':'0.003437',
'g7jac120':'378.0842',
'tube2':'0.001447',
'TSOPF_RS_b678_c1':'0.006199',
'watt_2':'0.00156',
'inlet_E':'1.6284',
'S10PI_n':'0.000012',
'S80PI_n1_E':'0.011741',
'rotor2':'0.019503',
'oscil_dcop_40':'0.085712',
'rotor1':'0.000013',
'crystk03':'0.003133',
'gyro':'0.053408',
'Reuters911_Day_64':'0.02333',
'utm1700b':'0.006861',
'as-caida':'0.054264',
't2d_q4_A_39':'0.00588',
'oscil_dcop_47':'0.085542',
't2d_q4_A_37':'0.319179',
't2d_q4_A_36':'0.006276',
't2d_q4_A_35':'0.00625',
't2d_q4_A_34':'0.006227',
't2d_q4_A_33':'0.006303',
't2d_q4_A_31':'0.006464',
't2d_q4_A_30':'0.00639',
'lhr34':'0.001486',
'c-19':'0.000052',
'c-18':'0.000044',
'extr1b':'0.017872',
'lund_b':'0.000018',
'oscil_dcop_07':'0.085464',
'oscil_dcop_06':'0.085786',
'oscil_dcop_05':'0.087003',
'oscil_dcop_04':'0.086717',
'oscil_dcop_03':'0.086252',
'oscil_dcop_02':'0.086617',
'oscil_dcop_01':'0.086724',
'oscil_dcop_09':'0.087159',
'jan99jac120sc':'19.58834',
'TSOPF_RS_b300_c1':'0.003537',
'igbt3':'0.034771',
't2d_q9_A_41':'0.011486',
'plbuckle':'0.00006',
'oscil_dcop_11':'11.57478',
'Zd_Jac2_db':'0.001087',
'jan99jac100':'0.47581',
'jazz':'0.00001',
'utm300':'0.10796',
'LFAT5':'0.000008',
't2d_q9_A_40':'0.006112',
'S20PI_n1_E':'0.016275',
'piston':'0.001295',
'plbuckle_G':'0.21133',
'vibrobox':'0.000808',
'hydr1c_A_58':'0.000201',
'nasa2910':'0.022636',
'ww_36_pmec_36':'0.000131',
'hydr1c_A_01':'0.000298',
'hydr1c_A_03':'0.000205',
'hydr1c_A_02':'0.016727',
'hydr1c_A_05':'0.000411',
'hydr1c_A_04':'0.000223',
'hydr1c_A_07':'0.000223',
'hydr1c_A_06':'0.000202',
'hydr1c_A_09':'0.000235',
'hydr1c_A_08':'0.000225',
'hydr1c_A_59':'0.000202',
't2d_q9_A_42':'0.006113',
'fpga_dcop_08':'17.17836',
'fpga_dcop_07':'0.16217',
'fpga_dcop_04':'0.15829',
'fpga_dcop_05':'0.16029',
'fpga_dcop_02':'0.15858',
'fpga_dcop_03':'0.15838',
'mark3jac100':'0.866779',
'shermanACb':'0.040958',
'shermanACa':'0.000187',
'TSOPF_RS_b300_c2':'0.004954',
'TSOPF_RS_b300_c3':'0.00734',
'adder_dcop_29':'0.000349',
't2d_q9_A_30':'0.006373',
'meg1':'0.000092',
'adder_dcop_23':'0.000416',
'adder_dcop_22':'0.00019',
'adder_dcop_21':'0.000181',
'adder_dcop_20':'0.016015',
'mark3jac080':'279.7814',
'swang1':'0.034928',
'swang2':'0.00055',
'west1505':'0.000022',
't2d_q9_A_38':'0.304457',
't2d_q9_A_39':'0.005875',
'add20':'0.004999',
'cbuckle_G':'4.3263',
't2d_q9_A_32':'0.006191',
't2d_q9_A_33':'0.006307',
't2d_q9_A_34':'0.006246',
't2d_q9_A_35':'0.006254',
't2d_q9_A_05':'0.012345',
'iprob':'0.029418',
'Si2':'0.000041',
'jan99jac100sc':'10.05608',
't2d_q9_A_16':'0.006394',
'adder_trans_01':'0.017312',
'Reuters911_Day_23':'0.000099',
'g7jac080sc':'245.4342',
'wathen120':'0.000884',
'viscoplastic1_C_7':'0.032767',
'ns3Da':'0.67185',
'viscoplastic1_C_5':'1.739036',
'd_ss':'0.011484',
'viscoplastic1_C_3':'0.033181',
'viscoplastic1_C_2':'0.040066',
'viscoplastic1_C_1':'0.024796',
'nmos3':'0.069358',
'g7jac040sc':'111.9564',
'utm3060':'0.014253',
'jan99jac060sc':'10.03244',
'viscoplastic1_C_4':'0.032908',
'SiH4':'0.000277',
'S40PI_n1_E':'0.019613',
'LF10000_M':'0.080768',
'poli':'0.000285',
't2d_q9_A_14':'0.006338',
'Reuters911_Day_54':'0.000071',
'circuit_1':'0.000563',
'powersim':'115.8297',
'std1_Jac2_db':'0.000756',
'mark3jac040':'144.0612',
'cage9':'0.00041',
'circuit_3':'88.98433',
'Zhao1':'0.004691',
'cage5':'0.000026',
'cage7':'0.000056',
'c-43':'0.000275',
't2d_q4_A_29':'0.006259',
't2d_q9_A_26':'0.006361',
't2d_q9_A_15':'0.006336',
'nasa2146':'0.000118',
'hydr1c':'0.032442',
'wathen100':'0.000942',
'mark3jac060':'212.0164',
'c-62':'0.06327',
'c-60':'0.000652',
'c-61':'0.000689',
't2d_q4_A_27':'0.006475',
'extr1b_A_28':'0.000181',
'extr1b_A_29':'0.000189',
'extr1b_A_26':'0.000366',
'extr1b_A_27':'0.000183',
'extr1b_A_24':'0.38177',
'extr1b_A_25':'0.000114',
'extr1b_A_22':'0.000095',
'extr1b_A_23':'0.00018',
'extr1b_A_20':'0.01297',
'extr1b_A_21':'0.37072',
'rajat05':'0.001065',
'rajat04':'0.058287',
'shyy41':'0.000059',
'lhr11c':'0.000334',
'hydr1c_A_40':'0.00035',
'LF10_E':'0.000009',
't2d_q4_A_45':'0.006173',
'memplus':'0.055465',
'airfoil_2d':'0.022197',
'cavity06':'0.024206',
'cavity07':'2.055075',
'cavity04':'0.22392',
'cavity05':'0.012323',
'cavity02':'0.003016',
'cavity03':'0.029578',
'cavity01':'0.000682',
'Ill_Stokes':'2.082097',
'cavity08':'0.10273',
'cavity09':'0.3173',
'lhr07c':'0.00034',
'adder_dcop_69':'0.023033',
'adder_dcop_68':'0.000664',
'adder_dcop_67':'0.000351',
'adder_dcop_64':'0.000398',
'ww_vref_6405':'0.017408',
'adder_dcop_61':'0.000477',
's2rmt3m1':'0.034724',
'n3c6-b1':'0.000011',
'n3c6-b7':'0.080625',
'c-27':'0.000095',
'b2_ss':'0.000078',
'heart3':'8.4108',
'jan99jac060':'0.364682',
'heart1':'26.128',
'nemeth03':'0.000712',
'lhr10c':'0.000487',
'Chebyshev1':'0.079631',
'Chebyshev3':'0.000701',
'Chebyshev2':'0.002548',
'goodwin':'121.8114',
'mark3jac020':'76.07818',
'hydr1c_A_52':'0.0002',
'hydr1c_A_53':'0.000183',
'hydr1c_A_50':'0.000356',
'hydr1c_A_51':'0.017203',
'hydr1c_A_56':'0.000191',
'hydr1c_A_57':'0.000204',
'hydr1c_A_54':'0.000221',
'hydr1c_A_55':'0.000187',
'nopss_11k':'0.568694',
't2d_q9':'0.00655',
'hydr1c_A_67':'0.000214',
'Na5':'0.000438',
't2d_q4':'0.34408',
'TSOPF_RS_b2052_c1':'0.011267',
'g7jac010':'0.055089',
'fpga_dcop_51':'17.17431',
'ted_B':'0.000298',
't3dl_a':'0.000978',
'CAG_mat364':'0.001115',
'poli_large':'0.001039',
't2d_q4_A_32':'0.006192',
'west0067':'0.000007',
'viscoplastic1_C_6':'0.032588',
't3dl':'0.043827',
'nopoly':'0.028213',
'zenios':'0.00004',
's3rmq4m1':'0.018681',
'Reuters911_Day_12':'0.021751',
'chipcool1_E':'0.03257',
't2d_q4_A_28':'0.006413',
'flowmeter5_E':'0.000086',
't2d_q4_A_25':'0.006342',
't2d_q4_A_26':'0.00643',
't2d_q4_A_20':'0.334142',
't2d_q4_A_21':'0.006594',
't2d_q4_A_22':'0.006444',
'cz40948':'333.2154',
'lhr02':'0.000095',
'lhr01':'0.000037',
'rajat13':'0.048531',
'lhr07':'0.000225',
'c-28':'0.000078',
'c-29':'0.000118',
'c-26':'0.000078',
'big':'2.5555',
'c-24':'0.000097',
'c-25':'0.000121',
'c-22':'0.00007',
'c-23':'0.000071',
'c-20':'0.000055',
'c-21':'0.00007',
't2d_q4_A_23':'0.006445',
'Reuters911':'0.000354',
'gyro_k':'0.001594',
'oscil_dcop_10':'0.085602',
'bips98_1450':'18.09195',
'oscil_dcop_12':'0.086823',
'oscil_dcop_13':'0.087757',
'oscil_dcop_14':'0.087849',
'oscil_dcop_15':'0.087111',
'oscil_dcop_16':'0.08514',
'oscil_dcop_17':'0.086484',
'oscil_dcop_18':'0.086829',
'oscil_dcop_19':'0.086428',
'invextr1_new':'29.591',
'CAG_mat1916':'0.0293',
'fv2':'0.000246',
'fv1':'0.000161',
'g7jac100':'314.2712',
'lhr14c':'0.000654',
'watt_1':'0.000918',
'oscil_dcop_46':'11.58181',
'young3c':'0.14184',
't2d_q9_A_06':'0.006491',
'rail_1357':'0.023687',
'rdist3a':'0.000092',
'extr1b_A_17':'0.37494',
'extr1b_A_16':'0.000189',
'extr1b_A_14':'0.000096',
'TSOPF_RS_b162_c3':'0.001141',
'extr1b_A_19':'0.37743',
'extr1b_A_18':'0.000094',
'hydr1c_A_16':'0.000224',
'hydr1c_A_14':'0.000206',
'football':'0.000006',
'hydr1c_A_12':'0.00021',
'adder_dcop_18':'0.000089',
'filter2D_E':'0.010305',
'hydr1c_A_11':'0.000254',
'bips07_3078':'0.049205',
'adder_dcop_19':'0.000174',
'hydr1c_A_18':'0.000202',
'hydr1c_A_19':'0.00025',
'fpga_dcop_19':'0.15895',
'fpga_dcop_18':'0.15778',
'wang2':'0.130403',
'onetone2':'0.000688',
'adder_dcop_17':'0.000088',
'ex28':'0.021035',
'fpga_dcop_12':'0.15735',
'adder_dcop_14':'0.000187',
'fpga_dcop_16':'17.17656',
'chem_master1':'15.42097',
'ted_A_unscaled':'160.113',
'TSOPF_RS_b39_c19':'0.034573',
't2dal':'0.0001',
'fv3':'0.020721',
't2dah':'0.000358',
'west0479':'0.000011',
'adder_dcop_35':'0.000271',
'mark3jac100sc':'0.003188',
'adder_dcop_30':'0.023064',
'adder_dcop_31':'0.000269',
'adder_dcop_32':'0.000504',
'wang1':'0.001407',
'adder_dcop_38':'0.000272',
'adder_dcop_39':'0.000262',
's3rmt3m3':'0.000386',
'flowmeter0_E':'0.02575',
'cbuckle':'0.032719',
'g7jac120sc':'378.4294',
'hydr1c_A_63':'0.000187',
'hydr1c_A_62':'0.000186',
'hydr1c_A_61':'0.000227',
'hydr1c_A_60':'0.017087',
'hydr1c_A_66':'0.000185',
'hydr1c_A_65':'0.000222',
't2d_q9_A_04':'0.006501',
'hydr1c_A_69':'0.000202',
'G61':'0.000094',
't2d_q9_A_01':'0.006481',
't2d_q9_A_02':'0.006581',
's1rmq4m1':'0.039526',
'lhr04c':'0.000125',
'cz2548':'2.924118',
'cell1':'56.64679',
'appu':'4.421133',
'cell2':'0.70276',
'pdb1HYS':'0.134905',
'qh1484':'0.000705',
'cz10228':'70.3564',
'cage8':'0.000126',
'west0497':'0.000018',
'TSOPF_RS_b39_c7':'0.000337',
'xingo_afonso_itaipu':'0.630927',
'extr1b_A_13':'0.000095',
't2d_q9_A_09':'0.006494',
'raefsky2':'0.013656',
'zeros_nopss_13k':'0.009516',
'pesa':'2.0776',
'cage6':'0.012658',
'jan99jac080sc':'0.47076',
'Reuters911_Day_47':'0.000081',
't520':'0.000602',
'mark3jac080sc':'279.78',
'GT01R':'0.004259',
'hydr1c_A_29':'0.000205',
'ex29':'0.020207',
't2d_q9_A_28':'0.006336',
't2d_q9_A_08':'0.006536',
'robot':'0.050883',
'LF10000':'0.036248',
'd_dyn':'0.000021',
't2d_q9_A_23':'0.006395',
't2dal_a':'0.00008',
'psmigr_2':'0.00071',
'oscil_dcop_29':'0.086952',
'oscil_dcop_28':'11.58117',
't2d_q9_A_20':'0.006632',
'cz1268':'0.012165',
'oscil_dcop_21':'0.086759',
'oscil_dcop_20':'0.085919',
'oscil_dcop_23':'0.086948',
'oscil_dcop_22':'0.086629',
'oscil_dcop_25':'0.086552',
'oscil_dcop_24':'0.085776',
'oscil_dcop_27':'0.085893',
'oscil_dcop_26':'0.085525',
'rajat14':'0.000052',
'rajat15':'0.003519',
'rajat11':'0.001176',
'rajat12':'0.002426',
'aft01':'0.027148',
'chipcool0_E':'0.000162',
'S10PI_n1_E':'0.00811',
'lhr14':'0.00097',
'd_dyn1':'0.011493',
'SiO':'0.071063',
'rajat19':'0.17685',
'west0989':'0.000017',
'cavity19':'0.59992',
'cavity18':'0.79127',
'jan99jac120':'1.603934',
'g7jac100sc':'314.4667',
'cavity11':'0.20657',
'cavity10':'3.574854',
'cavity13':'0.333',
'cavity12':'0.25624',
'cavity15':'1.0658',
'cavity14':'0.52393',
'cavity17':'0.79361',
'cavity16':'0.63559',
'jan99jac080':'0.421288',
't2d_q4_A_38':'0.011211',
't2d_q9_A_12':'0.012326',
'cage11':'0.005368',
'cage10':'0.001304',
'olafu':'0.001916',
'cryg10000':'1.3192',
't2d_q9_A_13':'0.00639',
't2d_q9_A_10':'0.00656',
'oscil_dcop_42':'0.088063',
't2d_q9_A_11':'0.333715',
'mark3jac060sc':'212.0599',
't2d_q4_A_04':'0.006479',
'extr1b_A_40':'28.57054',
'extr1b_A_41':'0.00019',
'extr1b_A_42':'0.36607',
'extr1b_A_43':'0.000095',
'extr1b_A_44':'0.000187',
't2d_q9_A_17':'0.006322'}
with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2default_Time433NoNoise.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = '/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2TimeComparisonWithDefault433NoNoise.csv'
		with open(csvoutput,'w') as csvout:
			writer = csv.writer(csvout)
			infile = csv.reader(csvinput)
			writer.writerow(header + ['default_time'] + ['Time Difference'] + ['Comparison'] + ['SpeedUp'])
			diff = 0.0
			bad=0
			good=0
			for row in infile:
				for key in default_time:
					if row[8] == key  :
						print(key)
						speed_up = float(default_time[key])/float(row[9])
						diff = round(((float(row[9]) - float(default_time[key]))/float(default_time[key]))*100,2)
						if diff>0:
							bad+=1
							ext = " % more time of default solver time"
						else:
							good+=1
							ext = " % less time of default solver time"
						diff1 = str(diff) + ext

						writer.writerow(row + [default_time[key]] + [diff] + [diff1] + [speed_up])
		print("We did better than default solver in ",good,"many cases out of ",(good+bad))
		print(good,bad, (good+bad))
		print("Result in file -->", csvoutput)

#writing only for good class labels
with open('/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2/RS2TimeComparisonWithDefault433NoNoise.csv', 'r+') as csvinput:
		infile = csv.reader(csvinput)
		csvoutput = '/Users/kanikas/Desktop/Feb22_MisPredictionAnalysis/3classlabelling/RS2TimeComparisonWithDefaultForGoodNoNoise.csv'
		with open(csvoutput,'w') as csvout:
			writer = csv.writer(csvout)
			infile = csv.reader(csvinput)
			writer.writerow(header + ['default_time'] + ['Time Difference'] + ['Comparison'] + ['SpeedUp'])
			for row in islice(infile, 1, None):
				if row[9] == "good" :
					writer.writerow(row)
		print("Result for only good labels in file -->", csvoutput)



 		