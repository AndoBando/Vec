{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


class AdditiveGroup v where
    {-#  MINIMAL zeroV, (<+>), ( negateV | (<->)) #-}
    zeroV   :: v
    (<+>)   :: v -> v -> v
    (<->)   :: v -> v -> v
    negateV :: v -> v

    {-# INLINE (<->)   #-}
    {-# INLINE negateV #-}

    negateV = (zeroV <->)
    (<->)   = (. negateV) . (<+>)

instance AdditiveGroup () where
  zeroV     = ()
  () <+> () = ()
  negateV   = id   

instance Num n => AdditiveGroup n where
    zeroV=0; (<+>) = (+); negateV = negate

instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a,b) where
    zeroV = (zeroV,zeroV)
    (a,b) <+> (a',b') = (a <+> a', b <+> b')
    negateV (a,b) = (negateV a, negateV b)