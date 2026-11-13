// Classifier Options (ecoClassify_ClassifierOptions): model type and sample size.

using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class ClassifierOptionsView : EcoClassifyViewBase
    {
        private const string COL_MODEL = "modelType";
        private const string COL_NOBS = "nObs";

        private readonly ComboBox m_Model;
        private readonly Label m_ModelNote;
        private readonly TextBox m_NObs;

        public ClassifierOptionsView()
        {
            // -- Model
            var g = this.AddGroup("Classifier");

            this.m_Model = ListBox(180);
            this.AddRow(g, "Model type", this.m_Model,
                "The algorithm used to classify each pixel as presence (1) or absence (0). " +
                "Change advanced settings (tuning, threshold, contextualization) under Advanced.");

            this.m_ModelNote = Note(string.Empty);
            this.AddFullRow(g, this.m_ModelNote);

            // -- Sampling
            g = this.AddGroup("Training sample",
                "Pixels are sampled from the training rasters in spatial blocks; 80% of blocks are used to train " +
                "the model and 20% are held out to test it and choose the threshold.");

            this.m_NObs = NumberBox(false, 100);
            this.AddRow(g, "Sample size (pixels)", this.m_NObs,
                "Total number of pixels sampled across all training rasters and timesteps, balanced between " +
                "presence and absence. Larger samples give more stable models but take longer to train. " +
                "Default 10000.");

            this.AddValidator(COL_NOBS, IntRange(10, int.MaxValue, "The sample size"));
        }

        protected override void BindControls()
        {
            this.SetComboBoxBinding(this.m_Model, COL_MODEL);
            this.SetTextBoxBinding(this.m_NObs, COL_NOBS);
        }

        protected override void UpdateDependentStates()
        {
            switch (this.m_Model.Text)
            {
                case "Random Forest":
                    this.m_ModelNote.Text =
                        "Random Forest (ranger): fast, robust default. Works well with many bands and covariates " +
                        "and reports variable importance.";
                    break;

                case "MaxEnt":
                    this.m_ModelNote.Text =
                        "MaxEnt (ENMeval + maxent.jar): presence/background model. Needs Java. Often better than " +
                        "Random Forest when presences are rare.";
                    break;

                case "CNN":
                    this.m_ModelNote.Text =
                        "CNN (torch): a small neural network. Slowest to train and needs the torch R package; " +
                        "can capture more complex relationships when there is plenty of training data.";
                    break;

                default:
                    this.m_ModelNote.Text = string.Empty;
                    break;
            }
        }
    }
}
