namespace TestApplication
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.txtData = new System.Windows.Forms.RichTextBox();
            this.btnVersion = new System.Windows.Forms.Button();
            this.btnModuleList = new System.Windows.Forms.Button();
            this.btnPVWatts = new System.Windows.Forms.Button();
            this.btnPVWattsFunc = new System.Windows.Forms.Button();
            this.btnArrayTest = new System.Windows.Forms.Button();
            this.btnTestMatrices = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // txtData
            // 
            this.txtData.Location = new System.Drawing.Point(12, 78);
            this.txtData.Name = "txtData";
            this.txtData.Size = new System.Drawing.Size(639, 422);
            this.txtData.TabIndex = 0;
            this.txtData.Text = "";
            // 
            // btnVersion
            // 
            this.btnVersion.Location = new System.Drawing.Point(26, 5);
            this.btnVersion.Name = "btnVersion";
            this.btnVersion.Size = new System.Drawing.Size(75, 23);
            this.btnVersion.TabIndex = 1;
            this.btnVersion.Text = "Version";
            this.btnVersion.UseVisualStyleBackColor = true;
            this.btnVersion.Click += new System.EventHandler(this.btnVersion_Click);
            // 
            // btnModuleList
            // 
            this.btnModuleList.Location = new System.Drawing.Point(107, 5);
            this.btnModuleList.Name = "btnModuleList";
            this.btnModuleList.Size = new System.Drawing.Size(95, 23);
            this.btnModuleList.TabIndex = 2;
            this.btnModuleList.Text = "Module List";
            this.btnModuleList.UseVisualStyleBackColor = true;
            this.btnModuleList.Click += new System.EventHandler(this.btnModuleList_Click);
            // 
            // btnPVWatts
            // 
            this.btnPVWatts.Location = new System.Drawing.Point(208, 5);
            this.btnPVWatts.Name = "btnPVWatts";
            this.btnPVWatts.Size = new System.Drawing.Size(75, 23);
            this.btnPVWatts.TabIndex = 3;
            this.btnPVWatts.Text = "PVWatts Example";
            this.btnPVWatts.UseVisualStyleBackColor = true;
            this.btnPVWatts.Click += new System.EventHandler(this.btnPVWatts_Click);
            // 
            // btnPVWattsFunc
            // 
            this.btnPVWattsFunc.Location = new System.Drawing.Point(289, 5);
            this.btnPVWattsFunc.Name = "btnPVWattsFunc";
            this.btnPVWattsFunc.Size = new System.Drawing.Size(110, 23);
            this.btnPVWattsFunc.TabIndex = 4;
            this.btnPVWattsFunc.Text = "PVWatts Func";
            this.btnPVWattsFunc.UseVisualStyleBackColor = true;
            this.btnPVWattsFunc.Click += new System.EventHandler(this.btnPVWattsFunc_Click);
            // 
            // btnArrayTest
            // 
            this.btnArrayTest.Location = new System.Drawing.Point(26, 35);
            this.btnArrayTest.Name = "btnArrayTest";
            this.btnArrayTest.Size = new System.Drawing.Size(75, 23);
            this.btnArrayTest.TabIndex = 5;
            this.btnArrayTest.Text = "Test Arrays";
            this.btnArrayTest.UseVisualStyleBackColor = true;
            this.btnArrayTest.Click += new System.EventHandler(this.btnArrayTest_Click);
            // 
            // btnTestMatrices
            // 
            this.btnTestMatrices.Location = new System.Drawing.Point(107, 34);
            this.btnTestMatrices.Name = "btnTestMatrices";
            this.btnTestMatrices.Size = new System.Drawing.Size(95, 23);
            this.btnTestMatrices.TabIndex = 6;
            this.btnTestMatrices.Text = "Test Matrices";
            this.btnTestMatrices.UseVisualStyleBackColor = true;
            this.btnTestMatrices.Click += new System.EventHandler(this.btnTestMatrices_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(663, 512);
            this.Controls.Add(this.btnTestMatrices);
            this.Controls.Add(this.btnArrayTest);
            this.Controls.Add(this.btnPVWattsFunc);
            this.Controls.Add(this.btnPVWatts);
            this.Controls.Add(this.btnModuleList);
            this.Controls.Add(this.btnVersion);
            this.Controls.Add(this.txtData);
            this.Name = "Form1";
            this.Text = "SSC Test Application";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.RichTextBox txtData;
        private System.Windows.Forms.Button btnVersion;
        private System.Windows.Forms.Button btnModuleList;
        private System.Windows.Forms.Button btnPVWatts;
        private System.Windows.Forms.Button btnPVWattsFunc;
        private System.Windows.Forms.Button btnArrayTest;
        private System.Windows.Forms.Button btnTestMatrices;
    }
}

