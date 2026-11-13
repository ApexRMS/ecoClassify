// Terminology (ecoClassify_Terminology, project scope): band label file, raster group name and
// the bands used for RGB preview images.

using System.Windows.Forms;

namespace ecoclassifyUI
{
    public class TerminologyView : EcoClassifyViewBase
    {
        private const string COL_BAND_FILE = "bandNames";
        private const string COL_GROUP = "timestepName";
        private const string COL_RED = "redBand";
        private const string COL_GREEN = "greenBand";
        private const string COL_BLUE = "blueBand";
        private const string COL_NIR = "nirBand";
        private const string COL_SWIR = "swirBand";

        private readonly TextBox m_BandFile;
        private readonly TextBox m_Group;
        private readonly TextBox m_Red;
        private readonly TextBox m_Green;
        private readonly TextBox m_Blue;
        private readonly TextBox m_Nir;
        private readonly TextBox m_Swir;

        public TerminologyView()
        {
            // -- Bands
            var g = this.AddGroup("Band names");

            Control picker = this.FilePicker(COL_BAND_FILE, "Band label file",
                "CSV files (*.csv)|*.csv|All files (*.*)|*.*", out this.m_BandFile);
            this.AddRow(g, "Band label file", picker,
                "A CSV with a header row and one band name per row, in band order. The number of names must " +
                "match the number of bands in the rasters. Applied when 'Override band names' is turned on " +
                "under Advanced. The file is copied into the library.");

            // -- Raster groups
            g = this.AddGroup("Raster groups");

            this.m_Group = TextField(160);
            this.AddRow(g, "Raster group name", this.m_Group,
                "What each numbered row of training/predicting rasters represents in this project (e.g. Timestep, " +
                "Image, Tile). Default: Timestep.");

            // -- RGB preview
            g = this.AddGroup("Band roles",
                "Band names as they appear in the rasters (or in the band label file, if one is used).");

            this.m_Red = TextField(160);
            this.AddRow(g, "Red band", this.m_Red, "Band shown as red in the RGB preview images.");

            this.m_Green = TextField(160);
            this.AddRow(g, "Green band", this.m_Green, "Band shown as green in the RGB preview images.");

            this.m_Blue = TextField(160);
            this.AddRow(g, "Blue band", this.m_Blue, "Band shown as blue in the RGB preview images.");

            this.AddFullRow(g, Note(
                "Set all three of red, green and blue to choose the bands in the RGB preview images. If any is " +
                "blank or not found, bands 3, 2 and 1 are used."));

            this.m_Nir = TextField(160);
            this.AddRow(g, "Near-infrared (NIR) band", this.m_Nir,
                "Optional. Recorded for spectral indices; not used by the current transformers.");

            this.m_Swir = TextField(160);
            this.AddRow(g, "Shortwave infrared (SWIR) band", this.m_Swir,
                "Optional. Recorded for spectral indices; not used by the current transformers.");
        }

        protected override void BindControls()
        {
            this.SetTextBoxBinding(this.m_BandFile, COL_BAND_FILE);
            this.SetTextBoxBinding(this.m_Group, COL_GROUP);
            this.SetTextBoxBinding(this.m_Red, COL_RED);
            this.SetTextBoxBinding(this.m_Green, COL_GREEN);
            this.SetTextBoxBinding(this.m_Blue, COL_BLUE);
            this.SetTextBoxBinding(this.m_Nir, COL_NIR);
            this.SetTextBoxBinding(this.m_Swir, COL_SWIR);
        }
    }
}
