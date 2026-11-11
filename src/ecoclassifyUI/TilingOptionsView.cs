// Tiling Options (ecoClassify_TilingOptions): how the predicting extent is split into tiles for
// spatial multiprocessing (used by the 2a-Prepare Spatial Tiling transformer).

using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class TilingOptionsView : EcoClassifyViewBase
    {
        private const string COL_AUTO = "AutoTiling";
        private const string COL_X = "NumTilesX";
        private const string COL_Y = "NumTilesY";

        private readonly CheckBox m_Auto;
        private readonly TextBox m_X;
        private readonly Label m_XLabel;
        private readonly TextBox m_Y;
        private readonly Label m_YLabel;
        private readonly Label m_Total;

        public TilingOptionsView()
        {
            var g = this.AddGroup("Spatial tiling",
                "Splits the predicting rasters into a grid of tiles so the Predict step can run tiles in parallel. " +
                "Only used when '2a-Prepare Spatial Tiling' is in the pipeline and multiprocessing is turned on " +
                "(Run Settings > Multiprocessing).");

            this.m_Auto = Toggle("Choose the tile grid from the number of multiprocessing jobs");
            this.AddFullRow(g, this.m_Auto,
                "On: about one tile per job, using the maximum jobs set under Run Settings > Multiprocessing " +
                "(or all CPU cores if that is not set).\r\nOff: use the tile counts below.");

            this.m_X = NumberBox(false, 60);
            this.m_XLabel = this.AddRow(g, "Tiles across (X)", this.m_X,
                "Number of tile columns. Defaults to 2 if blank.");

            this.m_Y = NumberBox(false, 60);
            this.m_YLabel = this.AddRow(g, "Tiles down (Y)", this.m_Y,
                "Number of tile rows. Defaults to 2 if blank.");

            this.m_Total = Note(string.Empty);
            this.AddFullRow(g, this.m_Total);

            this.m_X.TextChanged += (s, e) => this.UpdateDependentStates();
            this.m_Y.TextChanged += (s, e) => this.UpdateDependentStates();

            this.AddValidator(COL_X, IntRange(1, 1000, "The number of tiles across"));
            this.AddValidator(COL_Y, IntRange(1, 1000, "The number of tiles down"));
        }

        protected override void BindControls()
        {
            this.SetCheckBoxBinding(this.m_Auto, COL_AUTO);
            this.SetTextBoxBinding(this.m_X, COL_X);
            this.SetTextBoxBinding(this.m_Y, COL_Y);
        }

        protected override void UpdateDependentStates()
        {
            bool manual = !this.m_Auto.Checked;
            this.SetDependent(manual, this.m_X, this.m_XLabel, this.m_Y, this.m_YLabel);

            if (!manual)
            {
                this.m_Total.Text = "The tile grid is set at run time from the number of multiprocessing jobs.";
                return;
            }

            int x = int.TryParse(this.m_X.Text, out int xi) ? xi : 2;
            int y = int.TryParse(this.m_Y.Text, out int yi) ? yi : 2;
            this.m_Total.Text = string.Format("{0} x {1} = {2} tiles. Use about as many tiles as multiprocessing jobs.",
                x, y, x * y);
        }
    }
}
