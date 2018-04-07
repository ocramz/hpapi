#define hsc_calc_mask(minByte, minBit, mask, t, f)                    \
    {                                                                 \
        t x;                                                          \
        char *p = (char*)&x;                                          \
        memset(p,0,sizeof(t));                                        \
                                                                      \
        /* Find byte range */                                         \
        x.f = -1;                                                     \
        minByte = 0;                                                  \
        while (p[minByte] == 0) minByte++;                            \
        size_t maxByte = sizeof(t) - 1;                               \
        while (p[maxByte] == 0) maxByte--;                            \
                                                                      \
        /* Find bit range */                                          \
        x.f = 1;                                                      \
        size_t maxBit;                                                \
        if (p[minByte] != 0) {                                        \
            minBit = p[minByte];                                      \
            x.f = -1;                                                 \
            maxBit = p[maxByte] + 1;                                  \
        } else {                                                      \
            minBit = p[maxByte];                                      \
            x.f = -1;                                                 \
            maxBit = p[minByte] + 1;                                  \
        }                                                             \
                                                                      \
        mask = (maxBit * (1 << 8*(maxByte-minByte)) / minBit) - 1;    \
    }

#define hsc_peek_bit(t, f)                                            \
    {                                                                 \
        size_t minByte, minBit, mask;                                 \
        hsc_calc_mask(minByte,minBit,mask,t,f)                        \
        printf("(\\hsc_ptr -> peekByteOff hsc_ptr %lu"                \
               ">>= \\hsc_y -> return $ hsc_y `div` %lu .&. %lu)",    \
               minByte, minBit, mask);                                \
    }

#define hsc_poke_bit(t, f)                                            \
    {                                                                 \
        size_t minByte, minBit, mask;                                 \
        hsc_calc_mask(minByte,minBit,mask,t,f)                        \
        printf("(\\hsc_ptr hsc_value -> peekByteOff hsc_ptr %lu"      \
               ">>= \\hsc_y -> pokeByteOff hsc_ptr %lu"               \
               " ((hsc_y .&. %lu) .|. ((hsc_value * %lu) .&. %lu)))", \
               minByte, minByte, ~mask, minBit, mask);                \
    }
