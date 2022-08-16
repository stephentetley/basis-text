/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Basis/Text {
    
    pub enum DecimalFormatBuilder[r: Region, ka: Type, a: Type](MonadLib.Builder[r, ka, Interop/Text.DecimalFormat[r], a])

    instance Functor[DecimalFormatBuilder[r, ka]] {
        pub def map(f: a -> b \ ef, ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef = 
            Basis/Text/DecimalFormatBuilder.map(f, ma)
    }

    instance Applicative[DecimalFormatBuilder[r, ka]] {
        pub def point(x: a): DecimalFormatBuilder[r, ka, a] =  
            Basis/Text/DecimalFormatBuilder.point(x)

        pub def ap(mf: DecimalFormatBuilder[r, ka, a -> b \ ef], ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef =
            Basis/Text/DecimalFormatBuilder.ap(mf, ma)
    }

    instance Monad[DecimalFormatBuilder[r, ka]] {
        pub def flatMap(f: a -> DecimalFormatBuilder[r, ka, b] \ ef, ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef =
            Basis/Text/DecimalFormatBuilder.flatMap(f, ma)
    }


}


namespace Basis/Text/DecimalFormatBuilder {


    use Basis/Text.DecimalFormatBuilder;
    use Basis/Text.DecimalFormatBuilder.{DecimalFormatBuilder};
    use Basis/Math/RoundingMode.RoundingMode;
    

    pub def getDecimalFormatBuilder(x: DecimalFormatBuilder[r, ka, a]): MonadLib.Builder[r, ka, Interop/Text.DecimalFormat[r], a] = 
        let DecimalFormatBuilder(x1) = x;
        x1

    pub def point(x: a): DecimalFormatBuilder[r, ka, a] = DecimalFormatBuilder(MonadLib/Builder.point(x))

    pub def map(f: a -> b \ ef, ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef =
        DecimalFormatBuilder(MonadLib/Builder.map(f, getDecimalFormatBuilder(ma)))

    pub def ap(mf: DecimalFormatBuilder[r, ka, a -> b \ ef], ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef =
        DecimalFormatBuilder(MonadLib/Builder.ap(getDecimalFormatBuilder(mf), getDecimalFormatBuilder(ma)))


    pub def flatMap(f: a -> DecimalFormatBuilder[r, ka, b] \ ef, ma: DecimalFormatBuilder[r, ka, a]): DecimalFormatBuilder[r, ka, b] \ ef =
        DecimalFormatBuilder(MonadLib/Builder.flatMap(f >> getDecimalFormatBuilder, getDecimalFormatBuilder(ma)))

    pub def throwError(msg: String): DecimalFormatBuilder[r, ka, a] =
        DecimalFormatBuilder(MonadLib/Builder.throwError(msg))

    // def liftSetter(f: Interop/Text.DecimalFormat[r] -> Interop/Text.DecimalFormat[r] \ ef): DecimalFormatBuilder[r, ka, Unit] \ ef =
    //     DecimalFormatBuilder(MonadLib/Builder.liftSetter(f))

    def liftSetterUnit(f: Interop/Text.DecimalFormat[r] -> Unit \ ef): DecimalFormatBuilder[r, ka, Unit] \ ef =
        DecimalFormatBuilder(MonadLib/Builder.liftSetterUnit(f))


    def liftSetterResultUnit(f: Interop/Text.DecimalFormat[r] -> Result[Unit, err] \ ef): DecimalFormatBuilder[r, ka, Unit] \ ef with ToString[err] =
        DecimalFormatBuilder(MonadLib/Builder.liftSetterResultUnit(f))

    // def liftGetter(f: Interop/Text.DecimalFormat[r] -> a \ ef): DecimalFormatBuilder[r, ka, a] \ ef =
    //     DecimalFormatBuilder(MonadLib/Builder.liftGetter(f))

    // def liftGetterResult(f: Interop/Text.DecimalFormat[r] -> Result[a, err] \ ef): DecimalFormatBuilder[r, ka, a] \ ef with ToString[err] =
    //     DecimalFormatBuilder(MonadLib/Builder.liftGetterResult(f))

    ///
    /// Returns the result of applying `ma` to the initial state `s`.
    ///
    def runBuilder(r: Region[r], 
                        ma: DecimalFormatBuilder[r, a, a], 
                        s: Interop/Text.DecimalFormat[r]): Result[a, String] \ r = 
        MonadLib/Builder.runBuilder(r, getDecimalFormatBuilder(ma), s)

    /// TODO - wrap ans
    pub def build(r: Region[r], ma: DecimalFormatBuilder[r, a, a]): Result[Interop/Text.DecimalFormat[r], String] \ { Write(r), IO } = 
        let state1 = Interop/Text/DecimalFormat.new(r) as \ { Write(r), IO };
        match runBuilder(r, ma, state1) {
            case Err(msg) => Err(msg)
            case Ok(_)    => Ok(state1)
        }


    pub def setGroupingSize!(sz: Int32): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setGroupingSize!(sz))

    pub def setGroupingUsed!(b: Bool): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/SubNumberFormat.toNumberFormat >> Interop/Text/NumberFormat.setGroupingUsed!(b))


    pub def setMaximumIntegerDigits!(n: Int32): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setMaximumIntegerDigits!(n))

    pub def setMinimumIntegerDigits!(n: Int32): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setMinimumIntegerDigits!(n))

    pub def setMaximumFractionDigits!(n: Int32): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setMaximumFractionDigits!(n))

    pub def setMinimumFractionDigits!(n: Int32): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setMinimumFractionDigits!(n))

    pub def setNegativePrefix!(s: String): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setNegativePrefix!(s))

    pub def setNegativeSuffix!(s: String): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setNegativeSuffix!(s))

    pub def setPositivePrefix!(s: String): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setPositivePrefix!(s))

    pub def setPositiveSuffix!(s: String): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        liftSetterUnit(Interop/Text/DecimalFormat.setPositiveSuffix!(s))


    pub def setRoundingMode!(mode: RoundingMode): DecimalFormatBuilder[r, ka, Unit] \ Write(r) =
        let mode1 = Basis/Math/RoundingMode.marshallRoundingMode(mode);
        liftSetterResultUnit(Interop/Text/SubNumberFormat.toNumberFormat >> Interop/Text/NumberFormat.setRoundingMode!(mode1))        

}
