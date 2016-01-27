module KNXd.Client.Internal where

{-
#define EIB_INVALID_REQUEST             0x0000
#define EIB_CONNECTION_INUSE            0x0001
#define EIB_PROCESSING_ERROR            0x0002
#define EIB_CLOSED                      0x0003
#define EIB_RESET_CONNECTION            0x0004

#define EIB_OPEN_BUSMONITOR             0x0010
#define EIB_OPEN_BUSMONITOR_TEXT        0x0011
#define EIB_OPEN_VBUSMONITOR            0x0012
#define EIB_OPEN_VBUSMONITOR_TEXT       0x0013
#define EIB_BUSMONITOR_PACKET           0x0014
#define EIB_BUSMONITOR_PACKET_TS        0x0015
#define EIB_OPEN_BUSMONITOR_TS          0x0016
#define EIB_OPEN_VBUSMONITOR_TS         0x0017

#define EIB_OPEN_T_CONNECTION           0x0020
#define EIB_OPEN_T_INDIVIDUAL           0x0021
#define EIB_OPEN_T_GROUP                0x0022
#define EIB_OPEN_T_BROADCAST            0x0023
#define EIB_OPEN_T_TPDU                 0x0024
#define EIB_APDU_PACKET                 0x0025
#define EIB_OPEN_GROUPCON               0x0026
#define EIB_GROUP_PACKET                0x0027

#define EIB_PROG_MODE                   0x0030
#define EIB_MASK_VERSION                0x0031
#define EIB_M_INDIVIDUAL_ADDRESS_READ   0x0032

#define EIB_M_INDIVIDUAL_ADDRESS_WRITE  0x0040
#define EIB_ERROR_ADDR_EXISTS           0x0041
#define EIB_ERROR_MORE_DEVICE           0x0042
#define EIB_ERROR_TIMEOUT               0x0043
#define EIB_ERROR_VERIFY                0x0044

#define EIB_MC_INDIVIDUAL               0x0049
#define EIB_MC_CONNECTION               0x0050
#define EIB_MC_READ                     0x0051
#define EIB_MC_WRITE                    0x0052
#define EIB_MC_PROP_READ                0x0053
#define EIB_MC_PROP_WRITE               0x0054
#define EIB_MC_PEI_TYPE                 0x0055
#define EIB_MC_ADC_READ                 0x0056
#define EIB_MC_AUTHORIZE                0x0057
#define EIB_MC_KEY_WRITE                0x0058
#define EIB_MC_MASK_VERSION             0x0059
#define EIB_MC_RESTART                  0x005a
#define EIB_MC_WRITE_NOVERIFY           0x005b
#define EIB_MC_PROG_MODE                0x0060
#define EIB_MC_PROP_DESC                0x0061
#define EIB_MC_PROP_SCAN                0x0062
#define EIB_LOAD_IMAGE                  0x0063

#define EIB_CACHE_ENABLE                0x0070
#define EIB_CACHE_DISABLE               0x0071
#define EIB_CACHE_CLEAR                 0x0072
#define EIB_CACHE_REMOVE                0x0073
#define EIB_CACHE_READ                  0x0074
#define EIB_CACHE_READ_NOWAIT           0x0075
#define EIB_CACHE_LAST_UPDATES          0x0076
-}
