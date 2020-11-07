#include <vector>
#include <string>
#include <gtest/gtest.h>

#include "6par_solve.h"

TEST(SixParSolve_6par_solve, NewMonoSiModules) {
    // Vmp, Imp, Voc, Isc, alpha_isc, beta_voc, gamma_pmp, Nser, Tref
    std::vector<std::vector<double>> datasheet_values {
            {31.8, 17.29, 38.1, 18.39, 0.007356, -0.09525, -0.34, 110, 43},     // TSM-DEG19C.20 550 Vertex
            {31.7, 17.29, 38.1, 18.39, 0.0007356, -0.09525, -0.34, 110, 43},    // Similar to  TSM-DEG19C.20
            {34.6, 17.34, 41.7, 18.42, 0.007368, -0.10425, -0.34, 120, 43},     // TSM-DEG20C.20-600 Vertex
            {34.85, 17.22, 41.4, 18.5, 0.0074, -0.11178, -0.35, 120, 44}        // RSM120-8-600BMDG
    };
    int tech_id = module6par::monoSi;

    std::vector<std::vector<double>> param_values {
            {1.517481, 18.403519, 0.0, 0.096842, 131.731711, 12.881045},
            {1.442603, 18.407991, 0.0, 0.112672, 115.168462, 1.547922},
            {1.654555, 18.433292, 0.0, 0.119647, 165.806458, 11.976963},
            {1.701710, 18.517933, 0.0, 0.079108, 81.607566, 10.558608}
    };

    for (size_t i = 0; i < datasheet_values.size(); i++) {
        auto mod = datasheet_values[i];
        auto param = param_values[i];

        double Vmp = mod[0];
        double Imp = mod[1];
        double Voc = mod[2];
        double Isc = mod[3];
        double alpha_isc = mod[4];
        double beta_voc = mod[5];
        double gamma_pmp = mod[6];
        int Nser = (int)mod[7];
        double Tref = mod[8];

        module6par m( tech_id, Vmp, Imp, Voc, Isc, beta_voc, alpha_isc, gamma_pmp, Nser, Tref+273.15 );
        int err = m.solve_with_sanity_and_heuristics<double>(300,1e-7);
        EXPECT_GT(err, -1);
        EXPECT_NEAR(m.a, param[0], 1e-3);
        EXPECT_NEAR(m.Il, param[1], 1e-3);
        EXPECT_NEAR(m.Io, param[2], 1e-3);
        EXPECT_NEAR(m.Rs, param[3], 1e-3);
        EXPECT_NEAR(m.Rsh, param[4], 1e-3);
        EXPECT_NEAR(m.Adj, param[5], 1e-3);
    }
}


TEST(SixParSolve_6par_solve, CIGSModules) {
    // Vmp, Imp, Voc, Isc, alpha_isc, beta_voc, gamma_pmp, Nser, Tref
    std::vector<std::vector<double>> datasheet_values {
            {88.3, 1.7, 108.9, 1.83, 0.000183, -0.29403, -0.32, 96, 42},     // TSM-DEG19C.20 550 Vertex
    };
    int tech_id = module6par::CIGS;

    std::vector<std::vector<double>> param_values {
            {4.0131, 1.83478, 2.9142e-12, 4.88972, 1871.05056, -10.92130}
    };

    for (size_t i = 0; i < datasheet_values.size(); i++) {
        auto mod = datasheet_values[i];
        auto param = param_values[i];

        double Vmp = mod[0];
        double Imp = mod[1];
        double Voc = mod[2];
        double Isc = mod[3];
        double alpha_isc = mod[4];
        double beta_voc = mod[5];
        double gamma_pmp = mod[6];
        int Nser = (int)mod[7];
        double Tref = mod[8];

        module6par m( tech_id, Vmp, Imp, Voc, Isc, beta_voc, alpha_isc, gamma_pmp, Nser, Tref+273.15 );
        int err = m.solve_with_sanity_and_heuristics<double>(300,1e-7);
        EXPECT_GT(err, -1);
        EXPECT_NEAR(m.a, param[0], 1e-3);
        EXPECT_NEAR(m.Il, param[1], 1e-3);
        EXPECT_NEAR(m.Io, param[2], 1e-3);
        EXPECT_NEAR(m.Rs, param[3], 1e-3);
        EXPECT_NEAR(m.Rsh, param[4], 1e-3);
        EXPECT_NEAR(m.Adj, param[5], 1e-3);
    }
}
