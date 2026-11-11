// Advanced (ecoClassify_AdvancedClassifierOptions): raster preprocessing, tuning, threshold,
// contextualization and seed. Class name kept as AdvancedForm so existing package.xml wiring
// (viewClassName="ecoclassifyUI.AdvancedForm") is unchanged.

using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class AdvancedForm : EcoClassifyViewBase
    {
        private const string COL_OVERRIDE_BANDS = "overrideBandnames";
        private const string COL_NORMALIZE = "normalizeRasters";
        private const string COL_DECIMALS = "rasterDecimalPlaces";
        private const string COL_TUNING = "modelTuning";
        private const string COL_OBJECTIVE = "tuningObjective";
        private const string COL_MANUAL_THRESH = "setManualThreshold";
        private const string COL_THRESH = "manualThreshold";
        private const string COL_CONTEXT = "applyContextualization";
        private const string COL_WINDOW = "contextualizationWindowSize";
        private const string COL_SEED = "setSeed";

        private readonly CheckBox m_OverrideBands;
        private readonly CheckBox m_Normalize;
        private readonly TextBox m_Decimals;
        private readonly CheckBox m_Tuning;
        private readonly CheckBox m_ManualThresh;
        private readonly TextBox m_Thresh;
        private readonly Label m_ThreshLabel;
        private readonly ComboBox m_Objective;
        private readonly Label m_ObjectiveLabel;
        private readonly CheckBox m_Context;
        private readonly TextBox m_Window;
        private readonly Label m_WindowLabel;
        private readonly TextBox m_Seed;

        public AdvancedForm()
        {
            // -- Raster preprocessing
            var g = this.AddGroup("Raster preprocessing",
                "Applied to the training and predicting rasters before sampling and prediction.");

            this.m_OverrideBands = Toggle("Override band names using the project's band label file");
            this.AddFullRow(g, this.m_OverrideBands,
                "Renames every band using the band label file set in Project > Terminology (one name per row, " +
                "in band order). Use this when rasters from different sources name the same bands differently. " +
                "If no band label file is set, band names are left unchanged and a warning is logged.");

            this.m_Normalize = Toggle("Normalize raster bands");
            this.AddFullRow(g, this.m_Normalize,
                "Rescales each band to the 0-1 range (min-max) before training and prediction. Useful when bands " +
                "have very different value ranges or when predicting rasters come from a different sensor or date.");

            this.m_Decimals = NumberBox(false, 60);
            this.AddRow(g, "Decimal places", this.m_Decimals,
                "Rounds raster values to this many decimal places before use. Fewer decimals reduce noise and " +
                "memory use. Leave blank to keep full precision.");

            // -- Model tuning
            g = this.AddGroup("Model tuning");

            this.m_Tuning = Toggle("Run automated hyperparameter tuning");
            this.AddFullRow(g, this.m_Tuning,
                "Random Forest: searches mtry, maximum depth and number of trees, keeping the lowest out-of-bag error.\r\n" +
                "MaxEnt: searches feature classes and regularization multipliers, keeping the highest Boyce index.\r\n" +
                "CNN: trains for more epochs with a larger batch size.\r\n" +
                "Much slower than a single fit with default settings.");

            // -- Threshold
            g = this.AddGroup("Probability threshold",
                "The cutoff that turns predicted probabilities into presence (1) / absence (0).");

            this.m_ManualThresh = Toggle("Set the threshold manually");
            this.AddFullRow(g, this.m_ManualThresh,
                "On: use the fixed threshold below.\r\nOff: choose the threshold automatically from the held-out " +
                "test data using the objective below.");

            this.m_Thresh = NumberBox(true, 60);
            this.m_ThreshLabel = this.AddRow(g, "Threshold (0-1)", this.m_Thresh,
                "Pixels with a predicted probability at or above this value are classified as presence.");

            this.m_Objective = ListBox(160);
            this.m_ObjectiveLabel = this.AddRow(g, "Optimization objective", this.m_Objective,
                "Statistic maximized when choosing the threshold automatically:\r\n" +
                "  Accuracy: proportion of pixels classified correctly\r\n" +
                "  Specificity / Sensitivity: favour correct absences / correct presences\r\n" +
                "  Precision: proportion of predicted presences that are real\r\n" +
                "  Balanced: balanced accuracy, (sensitivity + specificity) / 2\r\n" +
                "  Youden: sensitivity + specificity - 1 (default, good for unbalanced classes)");

            // -- Contextualization
            g = this.AddGroup("Contextualization");

            this.m_Context = Toggle("Add neighbourhood (moving window) covariates");
            this.AddFullRow(g, this.m_Context,
                "Adds covariates computed over a moving window around each pixel (local mean, standard deviation, " +
                "minimum, maximum and range of every band) plus the first two principal components of the bands, so the classifier " +
                "can use spatial context as well as each pixel's own values.");

            this.m_Window = NumberBox(false, 60);
            this.m_WindowLabel = this.AddRow(g, "Window size (cells)", this.m_Window,
                "Width of the square moving window, in cells. Must be an odd number of 3 or more (3, 5, 7, ...). " +
                "Defaults to 3 if blank.");

            // -- Reproducibility
            g = this.AddGroup("Reproducibility");

            this.m_Seed = NumberBox(false, 100);
            this.AddRow(g, "Random seed", this.m_Seed,
                "Seed for sampling and model fitting. Set a value to get identical results on every run; " +
                "leave blank for a different random draw each run.");

            this.AddValidator(COL_THRESH, DoubleRange(0.0, 1.0, "The probability threshold"));
            this.AddValidator(COL_DECIMALS, IntRange(0, 15, "Decimal places"));
            this.AddValidator(COL_WINDOW, v =>
            {
                if (!int.TryParse(v, out int w) || w < 3 || w % 2 == 0)
                {
                    return "The contextualization window size must be an odd whole number of 3 or more (3, 5, 7, ...).";
                }

                return null;
            });
        }

        protected override void BindControls()
        {
            this.SetCheckBoxBinding(this.m_OverrideBands, COL_OVERRIDE_BANDS);
            this.SetCheckBoxBinding(this.m_Normalize, COL_NORMALIZE);
            this.SetTextBoxBinding(this.m_Decimals, COL_DECIMALS);
            this.SetCheckBoxBinding(this.m_Tuning, COL_TUNING);
            this.SetComboBoxBinding(this.m_Objective, COL_OBJECTIVE);
            this.SetCheckBoxBinding(this.m_ManualThresh, COL_MANUAL_THRESH);
            this.SetTextBoxBinding(this.m_Thresh, COL_THRESH);
            this.SetCheckBoxBinding(this.m_Context, COL_CONTEXT);
            this.SetTextBoxBinding(this.m_Window, COL_WINDOW);
            this.SetTextBoxBinding(this.m_Seed, COL_SEED);
        }

        protected override void UpdateDependentStates()
        {
            bool manual = this.m_ManualThresh.Checked;

            this.SetDependent(manual, this.m_Thresh, this.m_ThreshLabel);
            this.SetDependent(!manual, this.m_Objective, this.m_ObjectiveLabel);
            this.SetDependent(this.m_Context.Checked, this.m_Window, this.m_WindowLabel);
        }
    }
}
