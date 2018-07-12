#include <gtest/gtest.h>
#include <lib_shared_inverter.h>

/**
* Shared Inverter Class test
*/

class sharedInverterTest : public ::testing::Test {
protected:
	SharedInverter* inv;
	sandia_inverter_t sinv;
	partload_inverter_t plinv;
	double c1[3] = { 200., 30., -0.3 };		/// Temp derate curve 1 at 200 Vdc, start temp 30 C, slope -0.3
	double c2[3] = { 300., 50., -0.1 };		/// Curve 2 at 300 Vdc, start temp 50 C, slope -0.1
	double c3[3] = { 400., 40., -0.2 };		/// Curve 3 at 400 Vdc, start temp 40 C, slope -0.2
	double e = 0.01;
public:
	void SetUp() {
		inv = new SharedInverter(0, 1, &sinv, &plinv);
	}
	void TearDown() {
		if (inv) delete inv;
	}
};

TEST_F(sharedInverterTest, setUpTempDerateTest1_lib_shared_inverter) {
	// case 1: voltages in order
	double V[2] = { 0,0 };
	double startC[3] = { -99,-99,-99 };
	double slope[3] = { 0,0,0 };
	EXPECT_TRUE(inv->setTempDerateCurves(c1, c2, c3)) << "set up temp derate case 1";
	inv->getTempDerateCurves(V, startC, slope);
	EXPECT_NEAR(V[0], 250, e) << "set up temp derate case 1";
	EXPECT_NEAR(V[1], 350, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[0], 30, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[1], 50, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[2], 40, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[0], -0.3, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[1], -0.1, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[2], -0.2, e) << "set up temp derate case 1";

	// case 2: voltages out of order
	EXPECT_TRUE(inv->setTempDerateCurves(c2, c3, c1)) << "set up temp derate case 2";
	inv->getTempDerateCurves(V, startC, slope);
	EXPECT_NEAR(V[0], 250, e) << "set up temp derate case 1";
	EXPECT_NEAR(V[1], 350, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[0], 30, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[1], 50, e) << "set up temp derate case 1";
	EXPECT_NEAR(startC[2], 40, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[0], -0.3, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[1], -0.1, e) << "set up temp derate case 1";
	EXPECT_NEAR(slope[2], -0.2, e) << "set up temp derate case 1";
}

TEST_F(sharedInverterTest, setUpTempDerateTest2_lib_shared_inverter) {
	// case 3: use 1 voltage curve
	double V[2] = { 0,0 };
	double startC[3] = { -99,-99,-99 };
	double slope[3] = { 0,0,0 };
	EXPECT_TRUE(inv->setTempDerateCurves(c1)) << "set up temp derate case 3";
	inv->getTempDerateCurves(V, startC, slope);
	EXPECT_NEAR(V[0], 0, e) << "set up temp derate case 3";
	EXPECT_NEAR(V[1], 0, e) << "set up temp derate case 3";
	EXPECT_NEAR(startC[0], 30, e) << "set up temp derate case 3";
	EXPECT_NEAR(startC[1], -99, e) << "set up temp derate case 3";
	EXPECT_NEAR(startC[2], -99, e) << "set up temp derate case 3";
	EXPECT_NEAR(slope[0], -0.3, e) << "set up temp derate case 3";
	EXPECT_NEAR(slope[1], 0, e) << "set up temp derate case 3";
	EXPECT_NEAR(slope[2], 0, e) << "set up temp derate case 3";

	// case 4: use 2 voltage curves
	EXPECT_TRUE(inv->setTempDerateCurves(c1, c2)) << "set up temp derate case 4";
	inv->getTempDerateCurves(V, startC, slope);
	EXPECT_NEAR(V[0], 250, e) << "set up temp derate case 4";
	EXPECT_NEAR(V[1], 0, e) << "set up temp derate case 4";
	EXPECT_NEAR(startC[0], 30, e) << "set up temp derate case 4";
	EXPECT_NEAR(startC[1], 50, e) << "set up temp derate case 4";
	EXPECT_NEAR(startC[2], -99, e) << "set up temp derate case 4";
	EXPECT_NEAR(slope[0], -0.3, e) << "set up temp derate case 4";
	EXPECT_NEAR(slope[1], -0.1, e) << "set up temp derate case 4";
	EXPECT_NEAR(slope[2], 0, e) << "set up temp derate case 4";
}

TEST_F(sharedInverterTest, tempDerateTest3Curves_lib_shared_inverter) {
	double pAC = 100.;
	double eff = 0.;
	double loss = 0.;
	double V = 200.;
	double T = 20.;
	inv->setTempDerateCurves(c1, c2, c3);

	// zero efficiency error case
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 100., e) << "zero efficiency error case";
	EXPECT_NEAR(eff, 0., e) << "zero efficiency error case";
	EXPECT_NEAR(loss, 0, e) << "zero efficiency error case";

	// zero power error case
	pAC = 0.;
	eff = 1.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 0, e) << "zero power error case";
	EXPECT_NEAR(eff, 1., e) << "zero power error case";
	EXPECT_NEAR(loss, 0, e) << "zero power error case";

	// no derate cases
	pAC = 100.;
	eff = 1.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 100, e) << "no derate case 1";
	EXPECT_NEAR(eff, 1, e) << "no derate case 1";
	EXPECT_NEAR(loss, 0, e) << "no derate case 1";

	V = 300.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 100, e) << "no derate case 2";
	EXPECT_NEAR(eff, 1, e) << "no derate case 2";
	EXPECT_NEAR(loss, 0, e) << "no derate case 2";

	V = 400.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 100, e) << "no derate case 3";
	EXPECT_NEAR(eff, 1, e) << "no derate case 3";
	EXPECT_NEAR(loss, 0, e) << "no derate case 3";

	// curve one { 200., 30., -0.3 } tests
	V = 150.;
	T = 31.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 70, e) << "curve 1 used case 1";
	EXPECT_NEAR(eff, .7, e) << "curve 1 used case 1";
	EXPECT_NEAR(loss, 30, e) << "curve 1 used case 1";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	V = 210.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 70, e) << "curve 1 used case 2";
	EXPECT_NEAR(eff, .7, e) << "curve 1 used case 2";
	EXPECT_NEAR(loss, 30, e) << "curve 1 used case 2";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	T = 34.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 0, e) << "curve 1 used with negative efficiency";
	EXPECT_NEAR(eff, 0, e) << "curve 1 used with negative efficiency";
	EXPECT_NEAR(loss, 100, e) << "curve 1 used with negative efficiency";

	// curve two { 300., 50., -0.1 } tests
	pAC = 100.;
	eff = 1.;
	loss = 0.;
	V = 260.;
	T = 51.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 90, e) << "curve 2 used case 1";
	EXPECT_NEAR(eff, .9, e) << "curve 2 used case 1";
	EXPECT_NEAR(loss, 10, e) << "curve 2 used case 1";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	V = 310.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 90, e) << "curve 2 used case 2";
	EXPECT_NEAR(eff, .9, e) << "curve 2 used case 2";
	EXPECT_NEAR(loss, 10, e) << "curve 2 used case 2";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	T = 61.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 0, e) << "curve 2 used with negative efficiency";
	EXPECT_NEAR(eff, 0, e) << "curve 2 used with negative efficiency";
	EXPECT_NEAR(loss, 100, e) << "curve 2 used with negative efficiency";

	// curve three { 400., 40., -0.2 } tests
	pAC = 100.;
	eff = 1.;
	loss = 0.;
	V = 360.;
	T = 41.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 80, e) << "curve 3 used case 1";
	EXPECT_NEAR(eff, .8, e) << "curve 3 used case 1";
	EXPECT_NEAR(loss, 20, e) << "curve 3 used case 1";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	V = 410.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 80, e) << "curve 3 used case 2";
	EXPECT_NEAR(eff, .8, e) << "curve 3 used case 2";
	EXPECT_NEAR(loss, 20, e) << "curve 3 used case 2";

	pAC = 100.;
	eff = 1.;
	loss = 0.;
	T = 46.;
	inv->calculateTempDerate(V, T, pAC, eff, loss);
	EXPECT_NEAR(pAC, 0, e) << "curve 3 used with negative efficiency";
	EXPECT_NEAR(eff, 0, e) << "curve 3 used with negative efficiency";
	EXPECT_NEAR(loss, 100, e) << "curve 3 used with negative efficiency";
}